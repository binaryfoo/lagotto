package io.github.binaryfoo.lagotto

import java.io.File

/**
 * Where a log was loaded from.
 */
trait SourceRef {
  def line: Int
  def name: String
  def at(line: Int): SourceRef
  override def toString: String = s"$name:$line"
}

/**
 * File and position log was loaded from.
 *
 * @param file Name of the file.
 * @param line Line number within the file.
 */
case class FileRef(file: File, line: Int = 0) extends SourceRef {
  def name = file.getName
  override def at(line: Int): SourceRef = this.copy(line = line)
}

case class StdInRef(line: Int = 0) extends SourceRef {
  override def name: String = "stdin"
  override def at(line: Int): SourceRef = this.copy(line = line)
}
