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

  it should "parse a query that matches an element by attributes which are quoted" in {
    val query = QueryParser.parse( """[id="para"]""")
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

  it should "parse a simple parent child relation" in {
    val query = QueryParser.parse("div > p")
    query should be(ParentChildRelation(parent = TagName("div"), child = TagName("p")))
  }

  it should "parse an attribute based parent child relation" in {
    val query = QueryParser.parse("[id=foo] > [id=bar]")
    query should be(ParentChildRelation(parent = Attributes(Map("id" -> "foo")), child = Attributes(Map("id" -> "bar"))))
  }

  it should "parse an attribute based on tag name and attributes for both parent and child" in {
    val query = QueryParser.parse("p[id=foo] > p[id=bar]")
    query should be(ParentChildRelation(
      parent = TagName("p") and Attributes(Map("id" -> "foo")),
      child = TagName("p") and Attributes(Map("id" -> "bar")))
    )
  }

  it should "parse an attribute based on tag and attributes for parent and just tag for child" in {
    val query = QueryParser.parse("div[id=foo] > p")
    query should be(ParentChildRelation(
      parent = TagName("div") and Attributes(Map("id" -> "foo")),
      child = TagName("p"))
    )

    val child = Node("p", "", Map(), Nil)
    val parent = Option(Node("div", "", Map("id" -> "foo"), List(child)))
    query.matches(parent, child)
  }
}
