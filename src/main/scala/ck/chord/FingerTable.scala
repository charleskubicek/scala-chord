package ck.chord


sealed case class FingerEntry(extent:Int, nodeId:NodeId)


object FingerTableBuilder {

  import Collections._

  def apply(nodeId: NodeId, m: Int, nodeIds: List[NodeId]): Seq[FingerEntry] = {
    nodeIds.size match {
      case 1 => Seq(FingerEntry(1, 0))
      case _ => buildFingers(nodeIds, m, nodeId)
    }
  }

  private def buildFingers(nodeIds: List[NodeId], m: Int, nodeId: NodeId): IndexedSeq[FingerEntry] = {
    val nodeRanges = buildNodeRanges(nodeIds)

    Range(0, m).map { i =>
        val fingerKey = nodeId + Math.pow(2, i).toInt

        val successor = nodeRanges
          .find(nr => nr.contains(fingerKey))
          .getOrElse(nodeRanges.last).to

        FingerEntry(fingerKey, successor)
    }
  }

  private def buildNodeRanges(nodeIds: List[NodeId]): List[NodeRange] = {
    (nodeIds.slidingPair.toList :+ Pair(nodeIds.last, nodeIds.head)).map(NodeRange.fromPair)
  }

  private object NodeRange{
    def fromPair(a:(Int, Int)):NodeRange = NodeRange(a._1, a._2)
  }

  private case class NodeRange(from:NodeId, to:NodeId){
    def contains(n:NodeId) = n > from && n <= to
  }

}

class FingerTable(nodeId: NodeId, m: Int, nodeIds: List[NodeId]) {

  import Collections._

  assert(nodeIds.size > 0)

  private val table: Seq[FingerEntry] = FingerTableBuilder(nodeId, m, nodeIds)

  def buildNodeRanges(nodeIds: List[NodeId]): List[Pair[NodeId, NodeId]] = {
    nodeIds.slidingPair.toList :+ Pair(nodeIds.last, nodeIds.head)
  }

  def findSuccessFor(n: Int): NodeId = table.size match {
    case 1 => table.head.nodeId
    case _ =>

      table.slidingPair.find { case (from, to) =>
          n > from.extent && n <= to.extent
      }.map(_._2.nodeId).getOrElse(table.head.nodeId)
  }

}
