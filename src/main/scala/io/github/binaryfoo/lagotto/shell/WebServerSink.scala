package io.github.binaryfoo.lagotto.shell

import java.awt.Desktop
import java.io._
import java.net.URI
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.reader.FileIO
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Request, Server}
import org.eclipse.jetty.util.IO

class WebServerSink(format: OutputFormat) extends Sink {

  private val port = 1984
  private val in = new PipedInputStream(64 * 1024)
  private val out = new PrintStream(new PipedOutputStream(in))
  private val delegate = new IncrementalSink(format, true, out)
  private val server = new SillyServer(in, port)
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
  }

  server.start()

}

class SillyServer(index: InputStream, port: Int) {
  private val server = new Server(port)

  private val OpenFileReq = "(/.+)".r

  server.setHandler(new AbstractHandler {
    override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
      request.getPathInfo match {
        case OpenFileReq(file) =>
          val from = request.getParameter("from").toInt
          val to = request.getParameter("to").maybeToInt()
          response.setContentType("text/plain")
          FileIO.copyLines(file, from, to, new PrintWriter(response.getOutputStream))
        case _ =>
          response.setContentType("text/html; charset=UTF-8")
          IO.copy(index, response.getOutputStream)
      }
      baseRequest.setHandled(true)
    }
  })

  def start() = server.start()
}
