package htmlparser

import org.scalatest.FlatSpec
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}

class QueryParserTest extends FlatSpec {

  "QueryParser" should "parse a query that matches only nodes by tag name" in {
    val query = QueryParser.parse("p")
    query should be(TagName("p"))

    val node = Node("p", "", Map(), Nil)
    query.matches(node) should be(true)
  }

}
