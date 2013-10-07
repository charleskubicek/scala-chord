package ck.chord


object Network {
  private var nodes = Map[NodeId, Node]()

  def add(nodesToAdd: Node*) {
    nodes = nodes ++ nodesToAdd.map(n => n.id -> n)
  }

  def get(id: NodeId): Option[Node] = nodes.get(id)
}
