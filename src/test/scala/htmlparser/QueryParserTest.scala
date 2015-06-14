package htmlparser

import org.scalatest.FlatSpec
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}

class QueryParserTest extends FlatSpec {

  "QueryParser" should "parse a query that matches only nodes by tag name" in {
    val query = QueryParser.parse("p")
    query should be(TagName("p"))

    val p = Node("p", "", Map(), Nil)
    query.matches(p) should be(true)
    val div = Node("div", "", Map(), Nil)
    query.matches(div) should be(false)
  }

  it should "parse a query that matches an element by attributes" in {
    val query = QueryParser.parse("[id=para]")
    query should be(Attributes(Map("id" -> "para")))

    val p = Node("p", "", Map("id" -> "para"), Nil)
    query.matches(p) should be(true)
    val div = Node("div", "", Map("id" -> "para2"), Nil)
    query.matches(div) should be(false)
  }

  it should "parse a query that matches elements by tagname and attributes" in {
    val query = QueryParser.parse("p[id=para]")
    query should be(TagName("p") and Attributes(Map("id" -> "para")))

    val p = Node("p", "", Map("id" -> "para"), Nil)
    query.matches(p) should be(true)
    val div = Node("div", "", Map("id" -> "para2"), Nil)
    query.matches(div) should be(false)
  }
}
