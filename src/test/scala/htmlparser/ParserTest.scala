package htmlparser

import org.scalatest.FlatSpec
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}

class ParserTest extends FlatSpec {

  val sampleHTML =
    """
      |<html>
      |<head>
      |<title>First HTML Page</title>
      |</head>
      |<body>
      |<p id="header">Hello World!</p>
      |<div id="content"><p class="para">from Indix.</p></div>
      |</body>
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

  it should "parse a tag with attributes" in {
    val html =
      """
        |<p class="para">Hello World!</p>
      """.stripMargin
    val node = Node("p", "Hello World!", Map("class" -> "para"), Nil)
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

  it should "parse the sample html" in {
    val document = Parser.parse(sampleHTML)
    val title = Node("title", "First HTML Page", Map(), Nil)
    val head = Node("head", "", Map(), List(title))

    val p = Node("p", "Hello World!", Map("id" -> "header"), Nil)
    val pInsideDiv = Node("p", "from Indix.", Map("class" -> "para"), Nil)
    val div = Node("div", "", Map("id" -> "content"), List(pInsideDiv))

    val body = Node("body", "", Map(), List(p, div))
    val html = Node("html", "", Map(), List(head, body))
    document should be(DOM(List(html)))
    document.text should be("Hello World! from Indix.")
  }

}
