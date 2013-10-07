package ck.chord


sealed case class NodeState(predecessor: Option[NodeId], successor: Option[NodeId])

object Node {

  def createRing = {
    addToNetwork{
      Node(0, NodeState(None, Some(0)))
    }

  }

  def createNode(id: NodeId) = {
    addToNetwork{
      Node(id, NodeState(None, None))
    }
  }

  private def addToNetwork(n:Node):Node={
    Network.add(n)
    n
  }
}


case class Node(id: NodeId, var state: NodeState) {

  def join(ringNode: Node) {
    val mySuccessor = ringNode.state.successor.get //ringNode.findSuccessorFor(id)
    this.state = NodeState(None, Some(mySuccessor))
  }

  /**
   * Every node runs stabilize() periodically to learn about newly joined nodes.
   *
   * Each time node n runs stabilize(), it asks its successor for the
   * successor’s predecessor p, and decides whether p should be n’s successor
   * instead.
   *
   * This would be the case if node p recently joined the system. In
   * addition, stabilize() notifies node n’s successor of n’s existence, giving
   * the successor the chance to change its predecessor to n.
   *
   * The successor does this only if it knows of no closer predecessor than n.
   */
  def stabalize(){

    lookUpMySuccessorsPredecessorId foreach{ succPred =>
    // if my successor's predecessor is not me then it has been recently added
    // inbetween me and my successor. update my successor to be it.
      if(succPred != id){
        state = state.copy(successor = Some(succPred))
      }
    }

    lookUpMySuccessor foreach { succ => succ.notify(this.id)}
  }

  def lookUpMySuccessor:Option[Node] = {
    for (id   <- state.successor;
         node <- Network.get(id))
         yield node
  }

  def lookUpMySuccessorsPredecessorId:Option[NodeId] = {
    for (succNode   <- lookUpMySuccessor;
         succPredId <- succNode.state.predecessor
    ) yield succPredId
  }

  // we've been told that we might have a new predecessor
  def notify(suspectedPredecessor: NodeId){
    if(state.predecessor.isEmpty || state.predecessor.get == this.id){
      state = state.copy(predecessor = Some(suspectedPredecessor))
    }
  }

  def findSuccessorNodeFor(idQuery: NodeId): NodeId = {
    if(id == idQuery) state.successor.get
    else {
      val mysuccessor: NodeId = state.successor.get
      val mySucessorNOde: Node = Network.get(mysuccessor).get
      mySucessorNOde.findSuccessorNodeFor(idQuery)
    }
  }
}