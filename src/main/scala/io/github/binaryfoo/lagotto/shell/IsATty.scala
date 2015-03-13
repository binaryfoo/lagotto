package io.github.binaryfoo.lagotto.shell

import org.fusesource.jansi.internal.CLibrary._

object IsATty {

  var enabled = true

  def apply(): Boolean = {
    // We must be on some unix variant..
    try {
      enabled && isatty(STDOUT_FILENO) == 1
    }
    catch {
      case ignore: NoClassDefFoundError =>
        ignore.printStackTrace()
        false
      case ignore: UnsatisfiedLinkError =>
        ignore.printStackTrace()
        false
    }
  }

  def main(args: Array[String]) {
    println("tty " + apply())
  }
}
