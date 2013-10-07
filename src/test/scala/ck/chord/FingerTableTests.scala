package ck.chord

import org.scalatest.{BeforeAndAfterEach, FunSpec}
import org.scalatest.matchers.ShouldMatchers

class FingerTableTests extends FunSpec with ShouldMatchers with BeforeAndAfterEach {

  it("should build table as described in the paper"){
    val table: Seq[FingerEntry] = FingerTableBuilder(8, 6, List(1, 8, 14, 21, 32, 38, 42, 48))
    table(0) should be(FingerEntry(9 , 14))
    table(1) should be(FingerEntry(10, 14))
    table(2) should be(FingerEntry(12, 14))
    table(3) should be(FingerEntry(16, 21))
    table(4) should be(FingerEntry(24, 32))
    table(5) should be(FingerEntry(40, 42))
  }

  it("should find correct node when one node is in the ring"){
    val table = new FingerTable(0, 6, List(0))
    table.findSuccessFor(0) should be(0)
  }

  it("should find correct node when two nodes are in the ring"){
    val table = new FingerTable(0, 6, List(0, 2))
    table.findSuccessFor(0) should be(2)
    table.findSuccessFor(1) should be(2)
    table.findSuccessFor(2) should be(2)
    table.findSuccessFor(3) should be(0)
  }
}
