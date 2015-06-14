package htmlparser

import scala.util.parsing.combinator.RegexParsers

sealed trait Query {
  def matches(node: Node): Boolean

  def and(another: Query) = And(this, another)
}

case class TagName(target: String) extends Query {
  def matches(node: Node): Boolean = node.name == target
}

case class Attributes(attributes: Map[String, String]) extends Query {
  def matches(node: Node): Boolean = attributes.forall {
    case (key, value) => node.attributes.contains(key) && node.attributes(key) == value
  }
}

private[htmlparser] case class And(left: Query, right: Query) extends Query {
  def matches(node: Node): Boolean = left.matches(node) && right.matches(node)
}

object QueryParser extends RegexParsers {
  def NAME = regex("[a-z]+".r)

  def EQUAL_TO = literal("=")

  def SPACES = literal(" ").*

  def VALUE = NAME <~ SPACES

  def QUOTED_VALUE = literal("\"") ~> VALUE <~ literal("\"")

  def ATTRIBUTE = literal("[") ~> NAME ~ EQUAL_TO ~ (VALUE | QUOTED_VALUE) <~ literal("]") ^^ { case name ~ _ ~ value => Attributes(Map(name -> value))}

  def TAG_NAME = NAME ^^ { case tag => TagName(tag)}

  def TAG_WITH_ATTRIBUTES = TAG_NAME ~ ATTRIBUTE ^^ { case tagName ~ attributes => tagName and attributes}

  def QUERY: QueryParser.Parser[Query] = ATTRIBUTE | TAG_WITH_ATTRIBUTES | TAG_NAME

  def parse(query: String): Query = parse(QUERY, query) match {
    case Success(result, _) => result
    case Failure(msg, next) =>
      val error = next.source.toString.substring(next.offset)
      throw new RuntimeException(msg + error)
  }

}
