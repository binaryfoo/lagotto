package io.github.binaryfoo.lagotto.dictionary

object FieldPath {

  def expand(path: String): Seq[String] = {
    val parts = split(path)
    parts.drop(1).foldLeft(parts.head.begin())((parts, path) => path.appendTo(parts))
  }

  def expandPair(pair: (String, String)): Seq[(String, String)] = pair match {
    case (key, value) => expand(key).map(k => (k, value))
  }

  private val tokenizer = """(?<=[0-9}])\.(?=[0-9{])""".r
  private val RangeExpr = """\{(\d+)..(\d+)\}""".r

  def split(path: String): Seq[Part] = {
    tokenizer.split(path).map {
      case s if s.forall(Character.isDigit) => Literal(s.toInt)
      case RangeExpr(start, end) => Range(start.toInt, end.toInt)
    }
  }

  private def addNode(prefix: String, node: Int): String = prefix + "." + node

  trait Part {
    def begin(): Seq[String]
    def appendTo(paths: Seq[String]): Seq[String]
  }

  case class Literal(value: Int) extends Part {
    override def begin(): Seq[String] = Seq(value.toString)
    override def appendTo(paths: Seq[String]): Seq[String] = paths.map(addNode(_, value))

  }

  case class Range(start: Int, end: Int) extends Part {
    override def begin(): Seq[String] = (start to end).map(_.toString)
    override def appendTo(paths: Seq[String]): Seq[String] = (start to end).flatMap(i => paths.map(addNode(_, i)))
  }
}
