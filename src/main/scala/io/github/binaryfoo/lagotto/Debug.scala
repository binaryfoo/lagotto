package io.github.binaryfoo.lagotto

object Debug {

  var enabled = System.getProperty("lago.debug", "false").toBoolean

  def log(msg: => String): Unit = {
    if (enabled)
      Console.err.println(msg)
  }

}
