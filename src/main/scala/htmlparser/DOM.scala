package htmlparser

case class Node(name: String,
                innerText: String,
                attributes: Map[String, String],
                children: List[Node] = Nil) {
  def hasChildren = children.nonEmpty

  def id = attributes.get("id")

  def classes = attributes.get("class").map(_.split(","))

  def hasText = innerText != null && innerText.nonEmpty

  def text: String = innerText + children.map(_.text).mkString(" ")
}

case class DOM(nodes: List[Node]) {
  def text = nodes.map(_.text).mkString(" ").trim
}
