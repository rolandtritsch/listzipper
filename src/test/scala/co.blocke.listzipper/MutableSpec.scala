package co.blocke.listzipper.mutable

import org.scalatest.{ FunSpec, Matchers, PrivateMethodTester }

trait Thing {
  val name: String
}
case class Item(name: String) extends Thing
case class NotItem(name: String) extends Thing

class MutableSpec() extends FunSpec with Matchers with PrivateMethodTester {

  val getLeft = PrivateMethod[List[Int]]('left)
  val getFocus = PrivateMethod[List[Int]]('focus)
  val getRight = PrivateMethod[List[Int]]('right)

  describe("----------------\n:  Navigation  :\n----------------") {
    describe("Left Nav") {
      it("From off right edge") {
        val z = ListZipper(List(1, 2, 3, 4), None, Nil)
        z.moveLeft
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1, 2, 3), Some(4), Nil)
        )
      }
      it("From on right edge") {
        val z = ListZipper(List(1, 2, 3), Some(4), Nil)
        z.moveLeft
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1, 2), Some(3), List(4))
        )
      }
      it("To left edge") {
        val z = ListZipper(List(1, 2), Some(3), List(4))
        z.moveLeft.moveLeft
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (Nil, Some(1), List(2, 3, 4))
        )
      }
      it("Off left edge") {
        val z = ListZipper(Nil, Some(1), List(2, 3, 4))
        z.moveLeft
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (Nil, None, List(1, 2, 3, 4))
        )
        z.moveLeft.moveLeft
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (Nil, None, List(1, 2, 3, 4))
        )
      }
      it("While Left") {
        val z1 = ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7))
        z1.moveLeftWhile(_ > 1)
        (z1 invokePrivate getLeft(), z1 invokePrivate getFocus(), z1 invokePrivate getRight()) should be(
          (Nil, Some(1), List(2, 3, 4, 5, 6, 7))
        )
        val z2 = ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7))
        z2.moveLeftWhile(_ > 9)
        (z2 invokePrivate getLeft(), z2 invokePrivate getFocus(), z2 invokePrivate getRight()) should be(
          (List(1, 2, 3), Some(4), List(5, 6, 7))
        )
        val z3 = ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7))
        z3.moveLeftWhile(_ > -1)
        z3.crashedLeft should be(true)
      }
      it("While Right") {
        val z1 = ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7))
        z1.moveRightWhile(_ < 6)
        (z1 invokePrivate getLeft(), z1 invokePrivate getFocus(), z1 invokePrivate getRight()) should be(
          (List(1, 2, 3, 4, 5), Some(6), List(7))
        )
        val z2 = ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7))
        z2.moveRightWhile(_ < 1)
        (z2 invokePrivate getLeft(), z2 invokePrivate getFocus(), z2 invokePrivate getRight()) should be(
          (List(1, 2, 3), Some(4), List(5, 6, 7))
        )
        val z3 = ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7))
        z3.moveRightWhile(_ < 99)
        z3.crashedRight should be(true)
      }
    }
    describe("Right Nav") {
      it("From off right edge") {
        val z = ListZipper(Nil, None, List(1, 2, 3, 4))
        z.moveRight
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (Nil, Some(1), List(2, 3, 4))
        )
      }
      it("From on right edge") {
        val z = ListZipper(Nil, Some(1), List(2, 3, 4))
        z.moveRight
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1), Some(2), List(3, 4))
        )
      }
      it("To left edge") {
        val z = ListZipper(List(1), Some(2), List(3, 4))
        z.moveRight.moveRight
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1, 2, 3), Some(4), Nil)
        )
      }
      it("Off left edge") {
        val z = ListZipper(List(1, 2, 3), Some(4), Nil)
        z.moveRight
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1, 2, 3, 4), None, Nil)
        )
        z.moveRight.moveRight
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1, 2, 3, 4), None, Nil)
        )
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
        z.moveTo(-1)
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (Nil, None, List(1, 2, 3, 4, 5))
        )
        z.moveTo(0)
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (Nil, Some(1), List(2, 3, 4, 5))
        )
        z.moveTo(1)
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1), Some(2), List(3, 4, 5))
        )
        z.moveTo(4)
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1, 2, 3, 4), Some(5), Nil)
        )
        z.moveTo(5)
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1, 2, 3, 4, 5), None, Nil)
        )
      }
      it("first") {
        val z = ListZipper(List(1, 2, 3, 4))
        z.moveTo(2)
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1, 2), Some(3), List(4))
        )
        z.first
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (Nil, Some(1), List(2, 3, 4))
        )
        val z2 = ListZipper(List.empty[Int]).first
        (z2 invokePrivate getLeft(), z2 invokePrivate getFocus(), z2 invokePrivate getRight()) should be(
          (Nil, None, Nil)
        )
      }
      it("last") {
        val z = ListZipper(List(1, 2, 3, 4))
        z.moveTo(2)
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1, 2), Some(3), List(4))
        )
        z.last
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1, 2, 3), Some(4), Nil)
        )
        val z2 = ListZipper(List.empty[Int]).last
        (z2 invokePrivate getLeft(), z2 invokePrivate getFocus(), z2 invokePrivate getRight()) should be(
          (Nil, None, Nil)
        )
      }
    }
  }
  describe("--------------\n:  Mutation  :\n--------------") {
    describe("Modify") {
      it("From Empty") {
        val z = ListZipper(List.empty[Int]).modify(5)
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (Nil, Some(5), Nil)
        )
      }
      it("In focus") {
        val z = ListZipper(Nil, Some(1), Nil).modify(5)
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (Nil, Some(5), Nil)
        )
      }
      it("No focus") {
        val z = ListZipper(List(1), None, Nil).modify(5)
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1), None, Nil)
        )
      }
    }
    describe("Insertion") {
      it("Before") {
        val z = ListZipper(Nil, None, List(1, 2, 3)).insertBefore(5)
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (Nil, Some(5), List(1, 2, 3))
        )
        val z2 = ListZipper(Nil, Some(0), List(1, 2, 3)).insertBefore(5)
        (z2 invokePrivate getLeft(), z2 invokePrivate getFocus(), z2 invokePrivate getRight()) should be(
          (List(5), Some(0), List(1, 2, 3))
        )
        val z3 = ListZipper(List(8, 9), Some(0), List(1, 2, 3)).insertBefore(5)
        (z3 invokePrivate getLeft(), z3 invokePrivate getFocus(), z3 invokePrivate getRight()) should be(
          (List(8, 9, 5), Some(0), List(1, 2, 3))
        )
      }
      it("After") {
        val z = ListZipper(List(1, 2, 3), None, Nil).insertAfter(5)
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1, 2, 3), Some(5), Nil)
        )
        val z2 = ListZipper(List(1, 2, 3), Some(0), Nil).insertAfter(5)
        (z2 invokePrivate getLeft(), z2 invokePrivate getFocus(), z2 invokePrivate getRight()) should be(
          (List(1, 2, 3), Some(0), List(5))
        )
        val z3 = ListZipper(List(8, 9), Some(0), List(1, 2, 3)).insertAfter(5)
        (z3 invokePrivate getLeft(), z3 invokePrivate getFocus(), z3 invokePrivate getRight()) should be(
          (List(8, 9), Some(0), List(5, 1, 2, 3))
        )
      }
    }
    describe("Deletion") {
      it("deletes") {
        val z = ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7))
        z.delete.delete.delete
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1, 2, 3), Some(7), Nil)
        )
        z.delete
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (List(1, 2), Some(3), Nil)
        )
        z.delete.delete
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (Nil, Some(1), Nil)
        )
        z.delete
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (Nil, None, Nil)
        )
        z.delete.delete.delete
        (z invokePrivate getLeft(), z invokePrivate getFocus(), z invokePrivate getRight()) should be(
          (Nil, None, Nil)
        )
      }
    }
    describe("Merge") {
      it("left") {
        val z1 = ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7)).mergeLeft((a, b) => a * b)
        (z1 invokePrivate getLeft(), z1 invokePrivate getFocus(), z1 invokePrivate getRight()) should be(
          (List(1, 2), Some(12), List(5, 6, 7))
        )
        val z2 = ListZipper(List(3), Some(4), List(5, 6, 7)).mergeLeft((a, b) => a * b)
        (z2 invokePrivate getLeft(), z2 invokePrivate getFocus(), z2 invokePrivate getRight()) should be(
          (Nil, Some(12), List(5, 6, 7))
        )
        val z3 = ListZipper(Nil, Some(4), List(5, 6, 7)).mergeLeft((a, b) => a * b)
        (z3 invokePrivate getLeft(), z3 invokePrivate getFocus(), z3 invokePrivate getRight()) should be(
          (Nil, Some(4), List(5, 6, 7))
        )
        val z4 = ListZipper(Nil, None, List(5, 6, 7)).mergeLeft((a, b) => a * b)
        (z4 invokePrivate getLeft(), z4 invokePrivate getFocus(), z4 invokePrivate getRight()) should be(
          (Nil, None, List(5, 6, 7))
        )
      }
      it("leftAs") {
        val z1 = ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), List(Item("d"), Item("e")))
        z1.mergeLeftAs[Item]((a, b) => Item(a.name + b.name))
        (z1 invokePrivate getLeft(), z1 invokePrivate getFocus(), z1 invokePrivate getRight()) should be(
          (List(Item("a")), Some(Item("bc")), List(Item("d"), Item("e")))
        )

        val z2 = ListZipper[Thing](List(Item("a"), NotItem("b")), Some(Item("c")), List(Item("d"), Item("e")))
        z2.mergeLeftAs[Item]((a, b) => Item(a.name + b.name))
        (z2 invokePrivate getLeft(), z2 invokePrivate getFocus(), z2 invokePrivate getRight()) should be(
          (List(Item("a"), NotItem("b")), Some(Item("c")), List(Item("d"), Item("e")))
        )

        val z3 = ListZipper[Thing](List(Item("a"), Item("b")), Some(NotItem("c")), List(Item("d"), Item("e")))
        z3.mergeLeftAs[Item]((a, b) => Item(a.name + b.name))
        (z3 invokePrivate getLeft(), z3 invokePrivate getFocus(), z3 invokePrivate getRight()) should be(
          (List(Item("a"), Item("b")), Some(NotItem("c")), List(Item("d"), Item("e")))
        )

        val z4 = ListZipper[Thing](Nil, Some(Item("c")), List(Item("d"), Item("e")))
        z4.mergeLeftAs[Item]((a, b) => Item(a.name + b.name))
        (z4 invokePrivate getLeft(), z4 invokePrivate getFocus(), z4 invokePrivate getRight()) should be(
          (Nil, Some(Item("c")), List(Item("d"), Item("e")))
        )

        val z5 = ListZipper[Thing](Nil, None, List(Item("d"), Item("e")))
        z5.mergeLeftAs[Item]((a, b) => Item(a.name + b.name))
        (z5 invokePrivate getLeft(), z5 invokePrivate getFocus(), z5 invokePrivate getRight()) should be(
          (Nil, None, List(Item("d"), Item("e")))
        )
      }
      it("right") {
        val z1 = ListZipper(List(1, 2, 3), Some(4), List(5, 6, 7)).mergeRight((a, b) => a * b)
        (z1 invokePrivate getLeft(), z1 invokePrivate getFocus(), z1 invokePrivate getRight()) should be(
          (List(1, 2, 3), Some(20), List(6, 7))
        )
        val z2 = ListZipper(List(1, 2, 3), Some(4), List(5)).mergeRight((a, b) => a * b)
        (z2 invokePrivate getLeft(), z2 invokePrivate getFocus(), z2 invokePrivate getRight()) should be(
          (List(1, 2, 3), Some(20), Nil)
        )
        val z3 = ListZipper(List(1, 2, 3), Some(4), Nil).mergeRight((a, b) => a * b)
        (z3 invokePrivate getLeft(), z3 invokePrivate getFocus(), z3 invokePrivate getRight()) should be(
          (List(1, 2, 3), Some(4), Nil)
        )
        val z4 = ListZipper(List(1, 2, 3), None, Nil).mergeRight((a, b) => a * b)
        (z4 invokePrivate getLeft(), z4 invokePrivate getFocus(), z4 invokePrivate getRight()) should be(
          (List(1, 2, 3), None, Nil)
        )
      }
      it("rightAs") {
        val z1 = ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), List(Item("d"), Item("e")))
          .mergeRightAs[Item]((a, b) => Item(a.name + b.name))
        (z1 invokePrivate getLeft(), z1 invokePrivate getFocus(), z1 invokePrivate getRight()) should be(
          (List(Item("a"), Item("b")), Some(Item("cd")), List(Item("e")))
        )

        val z2 = ListZipper[Thing](List(Item("a"), Item("b")), Some(NotItem("c")), List(Item("d"), Item("e")))
        z2.mergeRightAs[Item]((a, b) => Item(a.name + b.name))
        (z2 invokePrivate getLeft(), z2 invokePrivate getFocus(), z2 invokePrivate getRight()) should be(
          (List(Item("a"), Item("b")), Some(NotItem("c")), List(Item("d"), Item("e")))
        )

        val z3 = ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), List(NotItem("d"), Item("e")))
        z3.mergeRightAs[Item]((a, b) => Item(a.name + b.name))
        (z3 invokePrivate getLeft(), z3 invokePrivate getFocus(), z3 invokePrivate getRight()) should be(
          (List(Item("a"), Item("b")), Some(Item("c")), List(NotItem("d"), Item("e")))
        )

        val z4 = ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), Nil)
        z4.mergeRightAs[Item]((a, b) => Item(a.name + b.name))
        (z4 invokePrivate getLeft(), z4 invokePrivate getFocus(), z4 invokePrivate getRight()) should be(
          (List(Item("a"), Item("b")), Some(Item("c")), Nil)
        )

        val z5 = ListZipper[Thing](List(Item("a"), Item("b")), None, Nil)
        z5.mergeRightAs[Item]((a, b) => Item(a.name + b.name))
        (z5 invokePrivate getLeft(), z5 invokePrivate getFocus(), z5 invokePrivate getRight()) should be(
          (List(Item("a"), Item("b")), None, Nil)
        )
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
      it("focusAs") {
        ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), List(Item("d"), Item("e"))).focusAs[Item] should be(Some(Item("c")))
        ListZipper[Thing](List(Item("a"), Item("b")), Some(Item("c")), List(Item("d"), Item("e"))).focusAs[NotItem] should be(None)
        ListZipper[Thing](Nil, None, List(Item("d"), Item("e"))).focusAs[Item] should be(None)
      }
    }
  }
}