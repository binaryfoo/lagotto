package io.github.binaryfoo.lagotto

/**
 * Where a log was loaded from.
 *
 * @param file Name of the file.
 * @param line Line number within the file.
 */
case class SourceRef(file: String, line: Int) {
  override def toString: String = s"$file:$line"
}
