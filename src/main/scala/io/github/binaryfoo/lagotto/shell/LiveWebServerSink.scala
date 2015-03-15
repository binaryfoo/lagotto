
package io.github.binaryfoo.lagotto.shell

import java.awt.Desktop
import java.io._
import java.net.URI
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import com.typesafe.config.ConfigFactory
import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.dictionary.{NameType, RootDataDictionary}
import io.github.binaryfoo.lagotto.highlight.HtmlMarkup
import io.github.binaryfoo.lagotto.reader.{FileIO, FileInProgress, LogTypes, SingleThreadLogReader}
import io.github.binaryfoo.lagotto.shell.OutputFormat.PipeToOutputFormatIterator
import io.github.binaryfoo.lagotto.shell.output.{DigestedFormat, NamedAndHighlightedFormat}
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Request, Server}
import org.eclipse.jetty.util.component.AbstractLifeCycle.AbstractLifeCycleListener
import org.eclipse.jetty.util.component.LifeCycle

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
 * Show results as they are produced.
 */
class LiveWebServerSink(format: OutputFormat) extends Sink {

  private val file = {
    val f = File.createTempFile("lago", ".out", new File("."))
    f.deleteOnExit()
    f
  }
  private val out = new PrintStream(new FileOutputStream(file))
  private val delegate = new IncrementalSink(format, true, out)
  private val result = new FileInProgress(file, contentType = format.contentType)
  private val server = new SillyServer(result, browseOnStart = false)
  private var launched = false
  private val launchFormat = format match {
    case FullText => "named"
    case _ => ""
  }

  override def entry(e: LogEntry) = {
    delegate.entry(e)
    if (!launched) {
      server.browseIndex(launchFormat)
      launched = true
    }
  }

  override def finish() = {
    delegate.finish()
    out.close()
    result.done = true
  }

  server.start()

}

/**
 * Show only the final file once it's baked.
 */
class OnFinishWebServerSink(index: String, contentType: ContentType) extends Sink {
  override def entry(e: LogEntry) = {}

  override def finish() = {
    val server = new SillyServer(index = new FileInProgress(new File(index), true, contentType))
    server.start()
  }
}

class SillyServer(index: FileInProgress, port: Int = 1984, browseOnStart: Boolean = true) {
  private val server = new Server(port)

  private val OpenFileReq = "(/.+)".r
  private val config = ConfigFactory.load()
  private lazy val dictionary = RootDataDictionary(config)
  private val autoDetectLog = LogTypes.auto(config)

  server.setHandler(new AbstractHandler {
    override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
      request.getPathInfo match {
        case "/favicon.ico" =>
        case "/" if request.getParameter("format") != null && index.contentType == PlainText =>
          val writer = formatted(request, response, StdInRef())
          FileIO.copy(new InputStreamReader(index.open()), writer)
          writer.close()
        case OpenFileReq(file) =>
          val from = request.getParameter("from").maybeToInt()
          val to = request.getParameter("to").maybeToInt()
          val foundFile = find(file)
          val writer = formatted(request, response, FileRef(new File(foundFile)))
          FileIO.copyLines(foundFile, from, to, writer)
        case _ =>
          response.setContentType(index.contentType.mimeType)
          FileIO.copy(index.open(), response.getOutputStream)
      }
      baseRequest.setHandled(true)
    }
  })

  private def formatted(request: HttpServletRequest, response: HttpServletResponse, sourceRef: SourceRef): PrintWriter = {
    val responseWriter = new PrintWriter(response.getOutputStream)
    formatFor(request.getParameter("format")) match {
      case Some(format) =>
        response.setContentType(format.contentType.mimeType)
        reformat(responseWriter, sourceRef, format)
      case None =>
        response.setContentType(PlainText.mimeType)
        responseWriter
    }
  }

  if (browseOnStart) {
    server.addLifeCycleListener(new AbstractLifeCycleListener {
      override def lifeCycleStarted(event: LifeCycle): Unit = browseIndex()
    })
  }

  def browseIndex(format: String = "") = {
    val url = if (format != "") {
      s"http://localhost:$port?format=$format"
    } else {
      s"http://localhost:$port"
    }
    Desktop.getDesktop.browse(new URI(url))
  }

  def reformat(writer: PrintWriter, source: SourceRef, format: OutputFormat): PrintWriter = {
    val out = new PipedOutputStream()
    val in = new PipedInputStream(out)
    val pipeJob = Future {
      val entries = SingleThreadLogReader(logType = autoDetectLog).read(in, source)
      entries.pipeTo(format, writer)
      writer.flush()
    }
    new PrintWriter(out) {
      override def close() = {
        super.close()
        Await.ready(pipeJob, 1.minute)
      }
    }
  }
  
  def formatFor(s: String): Option[OutputFormat] = s match {
    case "digest" => Some(DigestedFormat(dictionary, Some(NameType.English), HtmlMarkup))
    case "named" => Some(NamedAndHighlightedFormat(dictionary, HtmlMarkup))
    case "raw" | null => None
    case _ => throw new IAmSorryDave(s"Unknown format '$s'")
  }

  def find(file: String) = {
    if (new File(file).exists()) {
      file
    } else {
      file.substring(1)
    }
  }

  def start() = server.start()
}
