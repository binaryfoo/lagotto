package io.github.binaryfoo.lagotto.shell

import java.awt.Desktop
import java.io._
import java.net.URI
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import io.github.binaryfoo.lagotto.reader.{FileInProgress, FileIO}
import io.github.binaryfoo.lagotto._
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Request, Server}

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
          val from = request.getParameter("from").toInt
          val to = request.getParameter("to").maybeToInt()
          response.setContentType("text/plain")
          FileIO.copyLines(file, from, to, new PrintWriter(response.getOutputStream))
        case _ =>
          response.setContentType("text/html; charset=UTF-8")
          FileIO.copy(index.open(), response.getOutputStream)
      }
      baseRequest.setHandled(true)
    }
  })

  def start() = server.start()
}
