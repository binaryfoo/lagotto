package io.github.binaryfoo.lagotto.shell

import java.awt.Desktop
import java.io._
import java.net.URI
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import com.typesafe.config.ConfigFactory
import io.github.binaryfoo.lagotto.dictionary.{NameType, RootDataDictionary}
import io.github.binaryfoo.lagotto.reader.{SingleThreadLogReader, FileInProgress, FileIO}
import io.github.binaryfoo.lagotto.shell.OutputFormat.PipeToOutputFormatIterator
import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.shell.output.DigestedFormat
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Request, Server}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class WebServerSink(format: OutputFormat) extends Sink {

  private val port = 1984
  private val file = {
    val f = File.createTempFile("lago", ".out", new File("."))
    f.deleteOnExit()
    f
  }
  private val out = new PrintStream(new FileOutputStream(file))
  private val delegate = new IncrementalSink(format, true, out)
  private val result = new FileInProgress(file)
  private val server = new SillyServer(result, port)
  private var launched = false

  override def entry(e: LogEntry) = {
    delegate.entry(e)
    if (!launched) {
      Desktop.getDesktop.browse(new URI(s"http://localhost:$port"))
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

class SillyServer(index: FileInProgress, port: Int) {
  private val server = new Server(port)

  private val OpenFileReq = "(/.+)".r

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
          val writer = if (request.getParameter("digest") != null) {
            digestTo(responseWriter, foundFile)
          } else {
            responseWriter
          }
          FileIO.copyLines(foundFile, from, to, writer)
        case _ =>
          response.setContentType("text/html; charset=UTF-8")
          FileIO.copy(index.open(), response.getOutputStream)
      }
      baseRequest.setHandled(true)
    }
  })

  def digestTo(writer: PrintWriter, sourceName: String): PrintWriter = {
    val out = new PipedOutputStream()
    val in = new PipedInputStream(out)
    val pipeJob = Future {
      val entries = SingleThreadLogReader().read(in, FileRef(new File(sourceName)))
      entries.pipeTo(DigestedFormat(RootDataDictionary(ConfigFactory.load()), Some(NameType.English)), writer)
      writer.flush()
    }
    new PrintWriter(out) {
      override def close() = {
        super.close()
        Await.ready(pipeJob, 1.minute)
      }
    }
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
