package io.github.binaryfoo.lagotto

object Debug {

  var enabled = false

  def log(msg: => String): Unit = {
    if (enabled)
      Console.err.println(msg)
  }

}
