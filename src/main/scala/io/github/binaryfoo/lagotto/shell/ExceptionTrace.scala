package io.github.binaryfoo.lagotto.shell

import scala.annotation.tailrec

object ExceptionTrace {

  def messageTrace(root: Exception): String = trace(root, new StringBuilder)

  @tailrec
  private def trace(root: Throwable, b: StringBuilder, indent: String = ""): String = {
    b.append(indent).append(root.getMessage)
    if (root.getCause != null) {
      b.append('\n')
      trace(root.getCause, b, indent + "  ")
    } else {
      b.toString()
    }
  }
}
