package htmlparser

import scala.util.parsing.combinator.RegexParsers

sealed trait Query {
  def matches(parent: Option[Node], node: Node): Boolean

  def matches(node: Node): Boolean = matches(None, node)

  def and(another: Query) = And(this, another)
}

case class TagName(target: String) extends Query {
  def matches(parent: Option[Node], node: Node): Boolean = node.name == target
}

case class Attributes(attributes: Map[String, String]) extends Query {
  def matches(parent: Option[Node], node: Node): Boolean = attributes.forall {
    case (key, value) => node.attributes.contains(key) && node.attributes(key) == value
  }
}

case class ParentChildRelation(parent: Query, child: Query) extends Query {
  override def matches(parent: Option[Node], node: Node): Boolean = {
    val parentMatches = parent.exists(this.parent.matches)
    child match {
      case ParentChildRelation(me, grandChildrenQuery) => parentMatches && me.matches(node) && node.children.exists(grandChild => grandChildrenQuery.matches(Some(node), grandChild))
      case _ => parentMatches && child.matches(node)
    }
  }
}

private[htmlparser] case class And(left: Query, right: Query) extends Query {
  def matches(parent: Option[Node], node: Node): Boolean = left.matches(parent, node) && right.matches(parent, node)
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

  def RELATIONAL_QUERY = DIRECT_QUERY ~ literal(">") ~ DIRECT_QUERY ^^ { case parent ~ _ ~ child => ParentChildRelation(parent, child)}

  def NESTED_RELATIONAL_QUERY = DIRECT_QUERY ~ literal(">") ~ RELATIONAL_QUERY ^^ { case parent ~ _ ~ child => ParentChildRelation(parent, child)}

  def DIRECT_QUERY = ATTRIBUTE | TAG_WITH_ATTRIBUTES | TAG_NAME

  def QUERY: QueryParser.Parser[Query] = NESTED_RELATIONAL_QUERY | RELATIONAL_QUERY | DIRECT_QUERY

  def parse(query: String): Query = parse(QUERY, query) match {
    case Success(result, _) => result
    case Failure(msg, next) =>
      val error = next.source.toString.substring(next.offset)
      throw new RuntimeException(msg + error)
  }

}
