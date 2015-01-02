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

  val LeadingUpperCaseLetter = Rule("^([A-Z])", {m => Some(m.group(1).toLowerCase) })
  val DropIllegals = Rule("[^a-zA-Z0-9_ ]", {m => Some("") })
  val SpaceThenLetter = Rule(" +(\\w)", {m => Some(m.group(1).toUpperCase) })
  val LeadingDigit = Rule("^([0-9])", {m => Some(digits(m.group(1))) })

  val rules = Seq(LeadingUpperCaseLetter, DropIllegals, SpaceThenLetter, LeadingDigit)

  def toCamelCase(s: String): String = {
    var humped = s
    for (r <- rules)
      humped = r(humped)
    humped
  }

  case class Rule(regex: String, replacer: Regex.Match => Option[String]) {
    val r = regex.r.unanchored
    def apply(s: String): String = {
      r.replaceSomeIn(s, replacer)
    }
  }
}
