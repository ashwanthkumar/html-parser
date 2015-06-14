package htmlparser

import org.scalatest.FlatSpec
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}

class DOMTest extends FlatSpec {
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

  "DOM" should "parse and get the text for sampleHTML" in {
    HTMLParser.parse(sampleHTML).text should be("Hello World! from Indix.")
  }

  it should "parse and query for p" in {
    HTMLParser.parse(sampleHTML).query("p") should be("Hello World! from Indix.")
  }

  it should "parse and query for [id=\"header\"]" in {
    HTMLParser.parse(sampleHTML).query("[id=\"header\"]") should be("Hello World!")
  }

  it should "parse and query for [class=para]" in {
    HTMLParser.parse(sampleHTML).query("[class=para]") should be("from Indix.")
  }

  it should "parse and query body > p" in {
    HTMLParser.parse(sampleHTML).query("body > p") should be("Hello World!")
  }

  it should "parse and query div > p" in {
    HTMLParser.parse(sampleHTML).query("div > p") should be("from Indix.")
  }

  it should "parse and query body > div > p" in {
    HTMLParser.parse(sampleHTML).query("body > div > p") should be("from Indix.")
  }

  it should "parse and query [id=content] > p" in {
    HTMLParser.parse(sampleHTML).query("[id=content] > p") should be("from Indix.")
  }
}
