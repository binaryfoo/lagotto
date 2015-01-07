package io.github.binaryfoo.lagotto.dictionary

import scala.util.matching.Regex

object CamelCase {

  val digits = Map(
    "0" -> "zero",
    "1" -> "one",
    "2" -> "two",
    "3" -> "three",
    "4" -> "four",
    "5" -> "five",
    "6" -> "six",
    "7" -> "seven",
    "8" -> "eight",
    "9" -> "nine"
  )

  val LeadingUpperCaseLetter = WordRule("^([A-Z])", {m => Some(m.group(1).toLowerCase) })
  val DropIllegals = WordRule("[^a-zA-Z0-9_ ]", {m => Some("") })
  val SpaceThenLetter = WordRule(" +(\\w)", {m => Some(m.group(1).toUpperCase) })
  val LeadingDigit = WordRule("^([0-9])", {m => Some(digits(m.group(1))) })

  val rules = Seq(LeadingUpperCaseLetter, DropIllegals, SpaceThenLetter, LeadingDigit)

  def toCamelCase(s: String): String = {
    var humped = s
    for (r <- rules)
      humped = r(humped)
    humped
  }

}

object SentenceCase {

  val HeadCap = new WordRule("(?<=^.)(\\w+)", {m => Some(m.group(1).toLowerCase)})

  def toSentence(s: String): String = {
    s.split('_').map(HeadCap.apply).mkString(" ")
  }

}

case class WordRule(regex: String, replacer: Regex.Match => Option[String]) {
  val r = regex.r.unanchored
  def apply(s: String): String = {
    r.replaceSomeIn(s, replacer)
  }
}

