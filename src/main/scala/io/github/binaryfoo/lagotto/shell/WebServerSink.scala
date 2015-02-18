package io.github.binaryfoo.lagotto.shell

import java.awt.Desktop
import java.io._
import java.net.URI
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import io.github.binaryfoo.lagotto.{HttpHrefExpr, FileHrefExpr, LogEntry}
import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Request, Server}
import org.eclipse.jetty.util.IO

import scala.io.Source

class WebServerSink(format: OutputFormat) extends Sink {

  private val port = 1984
  private val baseUrl = s"http://localhost:$port"
  private val in = new PipedInputStream(64 * 1024)
  private val out = new PrintStream(new PipedOutputStream(in))
  private val delegate = new IncrementalSink(prepareHrefs(format), true, out)
  private val server = new SillyServer(in, port)

  private def prepareHrefs(format: OutputFormat) = format match {
    case t@Tabular(fields, _) => t.copy(fields = fields.map {
      case FileHrefExpr => HttpHrefExpr(baseUrl + "/log/")
      case f => f
    })
    case f => f
  }

  override def entry(e: LogEntry) = delegate.entry(e)

  override def finish() = {
    delegate.finish()
    out.close()
  }

  server.start()
  Desktop.getDesktop.browse(new URI(baseUrl))

}

class SillyServer(index: InputStream, port: Int) {
  private val server = new Server(port)

  private val OpenFileReq = "/log(/.*)".r

  server.setHandler(new AbstractHandler {
    override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
      request.getPathInfo match {
        case OpenFileReq(file) =>
          val from = request.getParameter("from").toInt
          val to = request.getParameter("to").toInt
          response.setContentType("text/plain")
          readLinesTo(file, from, to, new PrintWriter(response.getOutputStream))
        case _ => IO.copy(index, response.getOutputStream)
      }
    }
  })

  def readLinesTo(file: String, from: Int, to: Int, out: PrintWriter) = {
    val source = Source.fromFile(file)
    try {
      for (line <- source.getLines().drop(from).take(to - from)) {
        out.println(line)
      }
    }
    finally {
      source.close()
      out.close()
    }
  }

  def start() = server.start()
}
