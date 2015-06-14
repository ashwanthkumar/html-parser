package htmlparser

case class Node(name: String,
                text: String,
                attributes: Map[String, String],
                children: List[Node] = Nil) {
  def hasChildren = children.nonEmpty

  def id = attributes.get("id")

  def classes = attributes.get("class").map(_.split(","))
  
  def hasText = text != null && text.nonEmpty
}

case class DOM(nodes: List[Node])
