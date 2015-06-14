package htmlparser

case class Node(name: String,
                innerText: String,
                attributes: Map[String, String],
                children: List[Node] = Nil) {
  def hasChildren = children.nonEmpty

  def id = attributes.get("id")

  def classes = attributes.get("class").map(_.split(","))

  def hasText = ownText != null && ownText.nonEmpty

  def ownText = if (DOM.TEXT_BLACKLISTED_TAGS.contains(name)) "" else innerText

  def text: String = ownText + children.map(_.text).mkString(" ")
}

case class DOM(nodes: List[Node]) {
  def text = nodes.map(_.text).mkString(" ").trim
}

object DOM {
  val TEXT_BLACKLISTED_TAGS = List("title", "script", "link", "frame", "frameset", "iframe")
}
