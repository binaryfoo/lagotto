package io.github.binaryfoo.isotools.examples

import java.io.{File, FilenameFilter}

object DirFilter {
  def logsFrom(dir: String) = {
    new File(dir).listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(".log")
    })
  }
}
