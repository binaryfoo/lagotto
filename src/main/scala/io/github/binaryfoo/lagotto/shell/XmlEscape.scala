package io.github.binaryfoo.lagotto.shell

import java.io.PrintWriter

object XmlEscape {

  // from scala.xml.Utility.escape
  final def escape(text: String, s: PrintWriter): Unit = {
    val len = text.length
    var pos = 0
    while (pos < len) {
      text.charAt(pos) match {
        case '<' => s.append("&lt;")
        case '>' => s.append("&gt;")
        case '&' => s.append("&amp;")
        case '"' => s.append("&quot;")
        case '\n' => s.append('\n')
        case '\r' => s.append('\r')
        case '\t' => s.append('\t')
        case c => if (c >= ' ') s.append(c)
      }

      pos += 1
    }
  }
}
