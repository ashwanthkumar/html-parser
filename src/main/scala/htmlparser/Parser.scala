package htmlparser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Parser {

  def parse(html: String): DOM = DOM(HTMLParser.parse(html))
}

object HTMLParser extends RegexParsers {

  // Only skip new lines not white spaces
  override protected val whiteSpace: Regex = "\n".r

  def NAME = regex("[a-z]+".r)

  def START = literal("<") ~> NAME <~ literal(">")

  def CLOSE = literal("</") ~> NAME <~ literal(">")

  def TEXT = regex("[a-zA-Z0-9.!|@#$%^&*()_+={}\\[\\]\\ ]".r)

  def TAG = START ~ TEXT.* <~ CLOSE map { case tag ~ text => Node(tag, text.mkString, Map(), Nil)}

  def parse(html: String): List[Node] = parse(TAG.*, html) match {
    case Success(nodes, _) => nodes
    case Failure(msg, next) =>
      val error = next.source.toString.substring(next.offset)
      throw new RuntimeException(msg + error)
  }
}
