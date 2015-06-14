package htmlparser

import org.scalatest.FlatSpec
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}

class Parser$Test extends FlatSpec {

  val sampleHTML =
    """
      |<html>
      | <head>
      |  <title>First HTML Page</title>
      | </head>
      | <body>
      |  <p id="header">Hello World!</p>
      |  <div id="content"><p class="para">from Indix.</p></div>
      | </body>
      |</html>
    """.stripMargin

  "Parser" should "parse a single tag html without children" in {
    val html =
      """
        |<p>Hello World!</p>
      """.stripMargin

    val node = Node("p", "Hello World!", Map(), Nil)
    val document = Parser.parse(html)
    document should be(DOM(List(node)))
    document.text should be("Hello World!")
  }

  it should "parse 2 tags without children" in {
    val html =
      """
        |<p>Hello World!</p>
        |<span>from Indix.</span>
      """.stripMargin

    val node1 = Node("p", "Hello World!", Map(), Nil)
    val node2 = Node("span", "from Indix.", Map(), Nil)
    val document = Parser.parse(html)
    document should be(DOM(List(node1, node2)))
    document.text should be("Hello World! from Indix.")
  }
}
