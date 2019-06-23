package co.blocke.listzipper

import org.scalatest.{ FunSpec, Matchers }

trait Thing {
  val name: String
}
case class Item(name: String) extends Thing
case class NotItem(name: String) extends Thing

class ImmutableSpec() extends FunSpec with Matchers {

  describe("----------------\n:  Navigation  :\n----------------") {
    describe("Left Nav") {
      it("From off right edge") {
        val z = ListZipper(List(1, 2, 3, 4), None, Nil)
        z.moveLeft should be(ListZipper(List(1, 2, 3), Some(4), Nil))
      }
      it("From on right edge") {
        val z = ListZipper(List(1, 2, 3), Some(4), Nil)
        z.moveLeft should be(ListZipper(List(1, 2), Some(3), List(4)))
      }
      it("To left edge") {
        val z = ListZipper(List(1, 2), Some(3), List(4))
        z.moveLeft.moveLeft should be(ListZipper(Nil, Some(1), List(2, 3, 4)))
      }
      it("Off left edge") {
        val z = ListZipper(Nil, Some(1), List(2, 3, 4))
        z.moveLeft should be(ListZipper(Nil, None, List(1, 2, 3, 4)))
        z.moveLeft.moveLeft should be(ListZipper(Nil, None, List(1, 2, 3, 4)))
      }
      it("While Left") {
        val z = ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7))
        z.moveLeftWhile(_ > 1) should be(ListZipper(Nil, Some(1), List(2, 3, 4, 5, 6, 7)))
        z.moveLeftWhile(_ > 9) should be(ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7)))
        z.moveLeftWhile(_ > -1).crashedLeft should be(true)
      }
      it("While Right") {
        val z = ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7))
        z.moveRightWhile(_ < 6) should be(ListZipper(List(1, 2, 3, 4, 5), Some(6), List(7)))
        z.moveRightWhile(_ < 1) should be(ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7)))
        z.moveRightWhile(_ < 99).crashedRight should be(true)
      }
    }
    describe("Right Nav") {
      it("From off right edge") {
        val z = ListZipper(Nil, None, List(1, 2, 3, 4))
        z.moveRight should be(ListZipper(Nil, Some(1), List(2, 3, 4)))
      }
      it("From on right edge") {
        val z = ListZipper(Nil, Some(1), List(2, 3, 4))
        z.moveRight should be(ListZipper(List(1), Some(2), List(3, 4)))
      }
      it("To left edge") {
        val z = ListZipper(List(1), Some(2), List(3, 4))
        z.moveRight.moveRight should be(ListZipper(List(1, 2, 3), Some(4), Nil))
      }
      it("Off left edge") {
        val z = ListZipper(List(1, 2, 3), Some(4), Nil)
        z.moveRight should be(ListZipper(List(1, 2, 3, 4), None, Nil))
        z.moveRight.moveRight should be(ListZipper(List(1, 2, 3, 4), None, Nil))
      }
    }
    describe("Indexed") {
      it("Index value") {
        ListZipper(Nil, None, List(1, 2, 3)).index should be(-1)
        ListZipper(Nil, Some(1), List(2, 3)).index should be(0)
        ListZipper(List(1), Some(2), List(3)).index should be(1)
        ListZipper(List(1, 2), Some(3), Nil).index should be(2)
        ListZipper(List(1, 2, 3), None, Nil).index should be(-2)
      }
      it("Next") {
        ListZipper(Nil, Some(1), List(2, 3)).next should be(Some(2))
        ListZipper(List(1), Some(2), Nil).next should be(None)
        ListZipper(List(1, 2), None, Nil).next should be(None)
      }
      it("NextAs") {
        ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), List(Item("d"), Item("e"))).nextAs[Item] should be(Some(Item("d")))
        ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), List(NotItem("d"), Item("e"))).nextAs[Item] should be(None)
        ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), Nil).nextAs[Item] should be(None)
      }
      it("Prev") {
        ListZipper(Nil, Some(1), List(2, 3)).prev should be(None)
        ListZipper(Nil, None, List(1, 2, 3)).prev should be(None)
        ListZipper(List(1), Some(2), List(3)).prev should be(Some(1))
      }
      it("PrevAs") {
        ListZipper[Thing](Nil, Some(Item("c")), List(Item("d"), Item("e"))).prevAs[Item] should be(None)
        ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), List(Item("d"), Item("e"))).prevAs[Item] should be(Some(Item("b")))
        ListZipper[Thing](List(Item("a"), NotItem("b")), Some(Item("c")), List(Item("d"), Item("e"))).prevAs[Item] should be(None)
      }
      it("Moveto") {
        val z = ListZipper(List(1, 2), Some(3), List(4, 5))
        z.moveTo(-1) should be(ListZipper(Nil, None, List(1, 2, 3, 4, 5)))
        z.moveTo(0) should be(ListZipper(Nil, Some(1), List(2, 3, 4, 5)))
        z.moveTo(1) should be(ListZipper(List(1), Some(2), List(3, 4, 5)))
        z.moveTo(4) should be(ListZipper(List(1, 2, 3, 4), Some(5), Nil))
        z.moveTo(5) should be(ListZipper(List(1, 2, 3, 4, 5), None, Nil))
      }
    }
  }
  describe("--------------\n:  Mutation  :\n--------------") {
    describe("Modify") {
      it("From Empty") {
        ListZipper(List.empty[Int]).modify(5) should be(ListZipper(Nil, Some(5), Nil))
      }
      it("In focus") {
        ListZipper(Nil, Some(1), Nil).modify(5) should be(ListZipper(Nil, Some(5), Nil))
      }
      it("No focus") {
        ListZipper(List(1), None, Nil).modify(5) should be(ListZipper(List(1), None, Nil))
      }
    }
    describe("Insertion") {
      it("Before") {
        ListZipper(Nil, None, List(1, 2, 3)).insertBefore(5) should be(ListZipper(Nil, Some(5), List(1, 2, 3)))
        ListZipper(Nil, Some(0), List(1, 2, 3)).insertBefore(5) should be(ListZipper(List(5), Some(0), List(1, 2, 3)))
        ListZipper(List(8, 9), Some(0), List(1, 2, 3)).insertBefore(5) should be(ListZipper(List(8, 9, 5), Some(0), List(1, 2, 3)))
      }
      it("After") {
        ListZipper(List(1, 2, 3), None, Nil).insertAfter(5) should be(ListZipper(List(1, 2, 3), Some(5), Nil))
        ListZipper(List(1, 2, 3), Some(0), Nil).insertAfter(5) should be(ListZipper(List(1, 2, 3), Some(0), List(5)))
        ListZipper(List(8, 9), Some(0), List(1, 2, 3)).insertAfter(5) should be(ListZipper(List(8, 9), Some(0), List(5, 1, 2, 3)))
      }
    }
    describe("Deletion") {
      it("deletes") {
        val z = ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7))
        val z2 = z.delete.delete.delete
        z2 should be(ListZipper(List(1, 2, 3), Some(7), Nil))
        z2.delete should be(ListZipper(List(1, 2), Some(3), Nil))
        z2.delete.delete.delete should be(ListZipper(Nil, Some(1), Nil))
        z2.delete.delete.delete.delete should be(ListZipper(Nil, None, Nil))
        z2.delete.delete.delete.delete.delete should be(ListZipper(Nil, None, Nil))
      }
    }
    describe("Merge") {
      it("left") {
        ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7)).mergeLeft((a, b) => a * b) should be(ListZipper(List(1, 2), Some(12), List(5, 6, 7)))
        ListZipper(List(3), Some(4), List(5, 6, 7)).mergeLeft((a, b) => a * b) should be(ListZipper(Nil, Some(12), List(5, 6, 7)))
        ListZipper(Nil, Some(4), List(5, 6, 7)).mergeLeft((a, b) => a * b) should be(ListZipper(Nil, Some(4), List(5, 6, 7)))
        ListZipper(Nil, None, List(5, 6, 7)).mergeLeft((a, b) => a * b) should be(ListZipper(Nil, None, List(5, 6, 7)))
      }
      it("leftAs") {
        ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), List(Item("d"), Item("e"))).mergeLeftAs[Item]((a, b) => Item(a.name + b.name)) should be(ListZipper[Thing](List(Item("a")), Some(Item("bc")), List(Item("d"), Item("e"))))
        ListZipper[Thing](List(Item("a"), NotItem("b")), Some(Item("c")), List(Item("d"), Item("e"))).mergeLeftAs[Item]((a, b) => Item(a.name + b.name)) should be(ListZipper[Thing](List(Item("a"), NotItem("b")), Some(Item("c")), List(Item("d"), Item("e"))))
        ListZipper[Thing](List(Item("a"), Item("b")), Some(NotItem("c")), List(Item("d"), Item("e"))).mergeLeftAs[Item]((a, b) => Item(a.name + b.name)) should be(ListZipper[Thing](List(Item("a"), Item("b")), Some(NotItem("c")), List(Item("d"), Item("e"))))
        ListZipper[Thing](Nil, Some(Item("c")), List(Item("d"), Item("e"))).mergeLeftAs[Item]((a, b) => Item(a.name + b.name)) should be(ListZipper[Thing](Nil, Some(Item("c")), List(Item("d"), Item("e"))))
        ListZipper[Thing](Nil, None, List(Item("d"), Item("e"))).mergeLeftAs[Item]((a, b) => Item(a.name + b.name)) should be(ListZipper[Thing](Nil, None, List(Item("d"), Item("e"))))
      }
      it("right") {
        ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7)).mergeRight((a, b) => a * b) should be(ListZipper(List(1, 2, 3), Some(20), List(6, 7)))
        ListZipper(List(1, 2, 3), Some(4), List(5)).mergeRight((a, b) => a * b) should be(ListZipper(List(1, 2, 3), Some(20), Nil))
        ListZipper(List(1, 2, 3), Some(4), Nil).mergeRight((a, b) => a * b) should be(ListZipper(List(1, 2, 3), Some(4), Nil))
        ListZipper(List(1, 2, 3), None, Nil).mergeRight((a, b) => a * b) should be(ListZipper(List(1, 2, 3), None, Nil))
      }
      it("rightAs") {
        ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), List(Item("d"), Item("e"))).mergeRightAs[Item]((a, b) => Item(a.name + b.name)) should be(ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("cd")), List(Item("e"))))
        ListZipper[Thing](List(Item("a"), Item("b")), Some(NotItem("c")), List(Item("d"), Item("e"))).mergeRightAs[Item]((a, b) => Item(a.name + b.name)) should be(ListZipper[Thing](List(Item("a"), Item("b")), Some(NotItem("c")), List(Item("d"), Item("e"))))
        ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), List(NotItem("d"), Item("e"))).mergeRightAs[Item]((a, b) => Item(a.name + b.name)) should be(ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), List(NotItem("d"), Item("e"))))
        ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), Nil).mergeRightAs[Item]((a, b) => Item(a.name + b.name)) should be(ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), Nil))
        ListZipper[Thing](List(Item("a"), Item("b")), None, Nil).mergeRightAs[Item]((a, b) => Item(a.name + b.name)) should be(ListZipper[Thing](List(Item("a"), Item("b")), None, Nil))
      }
    }
    describe("toList") {
      it("Far Left") {
        ListZipper(Nil, None, List(1, 2, 3)).toList should be(List(1, 2, 3))
      }
      it("Far Right") {
        ListZipper(List(1, 2, 3), None, Nil).toList should be(List(1, 2, 3))
      }
      it("Middle") {
        ListZipper(List(1), Some(2), List(3)).toList should be(List(1, 2, 3))
      }
    }
    describe("Test Coverage") {
      it("coverage") {
        val z = ListZipper(List(1, 2, 3))
        z should be(ListZipper(Nil, Some(1), List(2, 3)))
        z.focus should be(Some(1))
        z.nonEmpty should be(true)
        ListZipper(Nil, Some(1), Nil).nonEmpty should be(true)
        ListZipper(Nil, None, List(1)).nonEmpty should be(true)
        z.size should be(3)
        ListZipper(Nil, None, Nil).size should be(0)
        ListZipper(Nil, Some(1), Nil).size should be(1)
      }
    }
  }
}