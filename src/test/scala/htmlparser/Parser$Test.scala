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

  "Parser" should "parse a tag" in {
    val html =
      """
        |<p>Hello World!</p>
      """.stripMargin

    val node = Node("p", "Hello World!", Map(), Nil)
    val document = Parser.parse(html)
    document should be(DOM(List(node)))
    document.text should be("Hello World!")
  }

  it should "parser multiple tags" in {
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

  it should "parse parent with a single children" in {
    val html =
      """
        |<div>
        |<p>Hello World!</p>
        |</div>
      """.stripMargin

    val child = Node("p", "Hello World!", Map(), Nil)
    val parent = Node("div", "", Map(), List(child))
    val document = Parser.parse(html)
    document should be(DOM(List(parent)))
    document.text should be("Hello World!")
  }

  it should "parse parent with multiple children" in {
    val html =
      """
        |<div>
        |<p>Hello World!</p>
        |<span>from Indix.</span>
        |</div>
      """.stripMargin

    val child1 = Node("p", "Hello World!", Map(), Nil)
    val child2 = Node("span", "from Indix.", Map(), Nil)
    val parent = Node("div", "", Map(), List(child1, child2))
    val document = Parser.parse(html)
    document should be(DOM(List(parent)))
    document.text should be("Hello World! from Indix.")
  }

}
