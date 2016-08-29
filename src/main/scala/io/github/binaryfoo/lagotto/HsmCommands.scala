package io.github.binaryfoo.lagotto

import scala.io.Source

object HsmCommands {

  val Commands: Set[String] = {
    val stream = Source.fromInputStream(HsmCommands.getClass.getClassLoader.getResourceAsStream("HsmCommands.txt"))
    try {
      (for {
        line <- stream.getLines() if !line.startsWith("#")
        Array(command, _*) = line.split(" +")
      } yield command).toSet
    } finally {
      stream.close()
    }
  }
}
