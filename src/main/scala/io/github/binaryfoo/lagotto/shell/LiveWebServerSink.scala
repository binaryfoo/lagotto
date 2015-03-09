
package io.github.binaryfoo.lagotto.shell

import java.awt.Desktop
import java.io._
import java.net.URI
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import com.typesafe.config.ConfigFactory
import io.github.binaryfoo.lagotto.dictionary.{NameType, RootDataDictionary}
import io.github.binaryfoo.lagotto.reader.{LogTypes, SingleThreadLogReader, FileInProgress, FileIO}
import io.github.binaryfoo.lagotto.shell.OutputFormat.PipeToOutputFormatIterator
import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.shell.output.{NamedAttributesFormat, DigestedFormat}
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Request, Server}
import org.eclipse.jetty.util.component.AbstractLifeCycle.AbstractLifeCycleListener
import org.eclipse.jetty.util.component.LifeCycle

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

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
  private val result = new FileInProgress(file)
  private val server = new SillyServer(result, browseOnStart = false)
  private var launched = false

  override def entry(e: LogEntry) = {
    delegate.entry(e)
    if (!launched) {
      server.browseIndex()
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
class OnFinishWebServerSink(index: String, contentType: String) extends Sink {
  override def entry(e: LogEntry) = {}

  override def finish() = {
    val server = new SillyServer(index = new FileInProgress(new File(index), true), indexContentType = contentType)
    server.start()
  }
}

class SillyServer(index: FileInProgress, port: Int = 1984, indexContentType: String = "text/html; charset=UTF-8", browseOnStart: Boolean = true) {
  private val server = new Server(port)

  private val OpenFileReq = "(/.+)".r
  private val config = ConfigFactory.load()
  private lazy val dictionary = RootDataDictionary(config)
  private val autoDetectLog = LogTypes.auto(config)

  server.setHandler(new AbstractHandler {
    override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
      request.getPathInfo match {
        case "/favicon.ico" =>
        case OpenFileReq(file) =>
          val from = request.getParameter("from").maybeToInt()
          val to = request.getParameter("to").maybeToInt()
          val foundFile = find(file)
          response.setContentType("text/plain")
          val responseWriter = new PrintWriter(response.getOutputStream)
          val writer = formatFor(request.getParameter("format"))
            .map(reformat(responseWriter, foundFile, _))
            .getOrElse(responseWriter)
          FileIO.copyLines(foundFile, from, to, writer)
        case _ =>
          response.setContentType(indexContentType)
          FileIO.copy(index.open(), response.getOutputStream)
      }
      baseRequest.setHandled(true)
    }
  })

  if (browseOnStart) {
    server.addLifeCycleListener(new AbstractLifeCycleListener {
      override def lifeCycleStarted(event: LifeCycle): Unit = browseIndex()
    })
  }

  def browseIndex() = {
    Desktop.getDesktop.browse(new URI(s"http://localhost:$port"))
  }

  def reformat(writer: PrintWriter, sourceName: String, format: OutputFormat): PrintWriter = {
    val out = new PipedOutputStream()
    val in = new PipedInputStream(out)
    val pipeJob = Future {
      val entries = SingleThreadLogReader(logType = autoDetectLog).read(in, FileRef(new File(sourceName)))
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
    case "digest" => Some(DigestedFormat(dictionary, Some(NameType.English)))
    case "named" => Some(NamedAttributesFormat(dictionary))
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
