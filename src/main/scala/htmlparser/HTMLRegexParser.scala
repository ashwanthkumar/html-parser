package htmlparser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object HTMLParser {

  def parse(html: String): DOM = DOM(HTMLRegexParser.parse(html))

}

object HTMLRegexParser extends RegexParsers {

  // Only skip new lines not white spaces
  override protected val whiteSpace: Regex = "\n".r

  def NAME = regex("[a-z]+".r)

  def EQUAL_TO = literal("=")

  def SPACES = literal(" ").*

  def VALUE = NAME <~ SPACES

  def QUOTED_VALUE = literal("\"") ~> VALUE <~ literal("\"")

  def ATTRIBUTE = (NAME ~ SPACES ~ EQUAL_TO ~ (VALUE | QUOTED_VALUE)) ^^ { case key ~ _ ~ _ ~ value => Map(key -> value)}

  def ATTRIBUTES = ATTRIBUTE.* ^^ {
    case attributes if attributes.nonEmpty => attributes.reduce(_ ++ _)
    case attributes => Map[String, String]()
  }

  def START_WITH_ATTRIBUTES = literal("<") ~> NAME ~ (SPACES ~> ATTRIBUTES) <~ literal(">") ^^ { case (name ~ attributes) => name -> attributes}

  def START_WITHOUT_ATTRIBUTES = literal("<") ~> NAME <~ literal(">") ^^ { case (name) => name -> Map[String, String]()}

  def START = START_WITH_ATTRIBUTES | START_WITHOUT_ATTRIBUTES

  def CLOSE = literal("</") ~> NAME <~ literal(">")

  def TEXT = regex("[a-zA-Z0-9.!|@#$%^&*()_+={}\\[\\]\\ ]".r).*

  def DOCUMENT = (NESTED_TAG | SINGLE_TAG).+ ^^ { case nodes => nodes.flatten}

  def NESTED_TAG: HTMLRegexParser.Parser[List[Node]] = START ~ DOCUMENT.+ <~ CLOSE ^^ { case (tag, attributes) ~ children => List(Node(tag, "", attributes, children.flatten))}

  def SINGLE_TAG: HTMLRegexParser.Parser[List[Node]] = START ~ TEXT <~ CLOSE ^^ { case (tag, attributes) ~ text => List(Node(tag, text.mkString, attributes, Nil))}

  def parse(html: String): List[Node] = parse(DOCUMENT, html) match {
    case Success(nodes, _) => nodes
    case Failure(msg, next) =>
      val error = next.source.toString.substring(next.offset)
      throw new RuntimeException(msg + error)
  }
}
