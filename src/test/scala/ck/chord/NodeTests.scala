package ck.chord

import org.scalatest.{BeforeAndAfterEach, FunSpec}
import org.scalatest.matchers.ShouldMatchers



class NodeTests extends FunSpec with ShouldMatchers with BeforeAndAfterEach {

  it("should start an ring with one node"){
    val onlyNode = Node.createRing

    onlyNode.state.predecessor should be(None)
    onlyNode.state.successor.get should be(0)

    onlyNode.stabalize()

    onlyNode.state.predecessor.get should be(0)
    onlyNode.state.successor.get should be(0)
  }

  /**
   * Key k is assigned to the first node whose identifier is
   * equal to or follows (the identifier of ) k in the
   * identifier space. This node is called the successor
   * node of key k, denoted by successor(k)
   *
   * successor(k) is the first node clockwise from k.
   */
  it("should find successor"){
    val n0 = Node(0, NodeState(Some(2), Some(1)))
    val n1 = Node(1, NodeState(Some(0), Some(2)))
    val n2 = Node(2, NodeState(Some(1), Some(0)))

    Network.add(n0, n1, n2)

    n0.findSuccessorNodeFor(0) should be(1)
    n1.findSuccessorNodeFor(1) should be(2)
    n2.findSuccessorNodeFor(2) should be(0)

    n0.findSuccessorNodeFor(1) should be(2)
    n0.findSuccessorNodeFor(2) should be(0)

    n1.findSuccessorNodeFor(0) should be(1)
  }

  implicit class NodeExtensions(n:Node){
    def predescesor:Int = n.state.predecessor.get
    def successor:Int = n.state.successor.get
  }

  it("should join a ring with one node"){
    val initialNode = Node.createRing

    // start and stabalize, node should have pred and succ pointing at it's self
    initialNode.state.predecessor should be(None)
    initialNode.successor should be(0)

    initialNode.stabalize()

    initialNode.predescesor should be(0)
    initialNode.successor   should be(0)


    // join, set successor
    val joiningNode = Node.createNode(1)
    joiningNode.join(initialNode)

    joiningNode.state.predecessor should be(None)
    joiningNode.successor         should be(0)

    // call stabalize on joined node. it should see it's succesor's
    // prdecessor it not it and call notify on node 0, setting it's
    // pred to 1
    joiningNode.stabalize()

    joiningNode.state.predecessor should be(None)
    joiningNode.successor         should be(0)

    initialNode.predescesor should be(1)
    initialNode.successor   should be(0)

    // call stabalize on initial node, should see it has new successor
    initialNode.stabalize()

    initialNode.predescesor should be(1)
    initialNode.successor   should be(1)

    joiningNode.predescesor should be(0)
    joiningNode.successor   should be(0)

  }
}
