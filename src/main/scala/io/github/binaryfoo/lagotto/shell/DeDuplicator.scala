package io.github.binaryfoo.lagotto.shell

import java.security.MessageDigest

import io.github.binaryfoo.lagotto.LogEntry

import scala.collection.mutable

class DeDuplicator extends (LogEntry => Boolean) {

  private val seen = new mutable.HashSet[String]()
  private val digest = MessageDigest.getInstance("SHA-256")

  override def apply(e: LogEntry): Boolean = seen.add(sha256(e.lines))

  private def sha256(s: String) = {
    val builder = new StringBuilder()
    for (b <- digest.digest(s.getBytes)) {
      builder.append(Integer.toHexString(b))
    }
    builder.toString()
  }
}
