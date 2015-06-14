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

  def query(queryStr: String) = {
    val query = QueryParser.parse(queryStr)
    find(query)(None, nodes).map(_.text).mkString(" ").trim
  }

  private final def find(query: Query)(parent: Option[Node], nodes: List[Node]): List[Node] = nodes match {
    case Nil => Nil
    case node :: rest if query.matches(parent, node) => List(node) ++ find(query)(Some(node), node.children) ++ find(query)(parent, rest)
    case node :: rest => find(query)(Some(node), node.children) ++ find(query)(parent, rest)
  }
}

object DOM {
  val TEXT_BLACKLISTED_TAGS = List("title", "script", "link", "frame", "frameset", "iframe")

  def apply(nodes: Node*): DOM = DOM(nodes.toList)
}
