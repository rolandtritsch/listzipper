package co.blocke.collection.mutable

object ListZipper {
  def apply[A](initial: Seq[A]): ListZipper[A] =
    if (initial.isEmpty)
      ListZipper(Nil, None, Nil)
    else
      ListZipper(Nil, Some(initial.head), initial.tail.toList)

  def empty[A]: ListZipper[A] = ListZipper(Nil, None, Nil)
}

case class ListZipper[A](private var left: List[A], private var curFocus: Option[A], private var right: List[A]) {

  def focus: Option[A] = curFocus

  def isEmpty: Boolean = left.isEmpty && right.isEmpty && curFocus.isEmpty
  def nonEmpty: Boolean = left.nonEmpty || right.nonEmpty || curFocus.isDefined
  def crashedLeft: Boolean = left.isEmpty && curFocus.isEmpty
  def crashedRight: Boolean = right.isEmpty && curFocus.isEmpty

  def size: Int = left.size + right.size + { if (curFocus.isDefined) 1 else 0 }
  def index: Int = left match {
    case Nil if curFocus.isDefined => 0
    case Nil                       => -1 // off left edge
    case _ if curFocus.isEmpty     => -2 // off right edge
    case _                         => left.size
  }

  def map[B](f: (A) => B): ListZipper[B] = {
    val ml = left.map(a => f(a))
    val mf = curFocus.map(a => f(a))
    val mr = right.map(a => f(a))
    ListZipper(ml, mf, mr)
  }

  def flatMap[B](f: (A) => IterableOnce[B]): ListZipper[B] = {
    val ml = left.flatMap(a => f(a))
    var ugly: List[B] = Nil
    val mf = curFocus.flatMap { a =>
      // we gotta spill over any new items to the right side, maintaining focus position
      val xformed = f(a).iterator.to(List)
      if (xformed.size > 1)
        ugly = xformed.drop(1)
      xformed.headOption
    }
    val mr = ugly ++ right.flatMap(a => f(a))
    ListZipper(ml, mf, mr)
  }

  def first: ListZipper[A] = {
    if (!isEmpty) {
      val all = this.toList
      left = Nil
      curFocus = Some(all.head)
      right = all.tail
    }
    this
  }
  def last: ListZipper[A] = {
    if (!isEmpty) {
      val all = this.toList
      left = all.take(all.size - 1)
      curFocus = Some(all.last)
      right = Nil
    }
    this
  }

  def toList: List[A] = left ++ { if (curFocus.isDefined) curFocus.get +: right else right }

  def moveTo(i: Int): ListZipper[A] = {
    val asList = toList
    val (l, r) = asList.splitAt(i)
    if (r.isEmpty) {
      left = l
      curFocus = None
      right = Nil
    } else if (i >= 0) {
      left = l
      curFocus = Some(r.head)
      right = r.tail
    } else {
      left = l
      curFocus = None
      right = r
    }
    this
  }

  def moveLeft: ListZipper[A] = left match {
    case Nil if curFocus.isDefined =>
      left = Nil
      right = curFocus.get +: right
      curFocus = None
      this
    case Nil => this // already off the left edge... do nothing
    case _ if curFocus.isEmpty =>
      curFocus = Some(left.last)
      left = left.take(left.length - 1)
      right = Nil
      this
    case _ =>
      right = curFocus.get +: right
      curFocus = Some(left.last)
      left = left.take(left.length - 1)
      this
  }

  def moveLeftWhile(fn: A => Boolean): ListZipper[A] = {
    while (curFocus.isDefined && fn(curFocus.get))
      moveLeft
    this
  }

  def moveRight: ListZipper[A] = right match {
    case Nil if curFocus.isDefined =>
      left = left :+ this.curFocus.get
      curFocus = None
      right = Nil
      this
    case Nil => this // already off the right edge... do nothing
    case _ if curFocus.isEmpty =>
      left = Nil
      curFocus = Some(right.head)
      right = right.tail
      this
    case _ =>
      left = left :+ this.curFocus.get
      curFocus = Some(right.head)
      right = right.tail
      this
  }

  def moveRightWhile(fn: A => Boolean): ListZipper[A] = {
    while (curFocus.isDefined && fn(curFocus.get))
      moveRight
    this
  }

  def modify(a: A): ListZipper[A] = {
    if (isEmpty || curFocus.isDefined)
      curFocus = Some(a)
    this // nothing changed...nothing in curFocus
  }

  def insertBefore(a: A): ListZipper[A] = {
    if (curFocus.isDefined)
      left = left :+ a
    else
      curFocus = Some(a)
    this
  }

  def insertAfter(a: A): ListZipper[A] = {
    if (curFocus.isDefined)
      right = a +: right
    else
      curFocus = Some(a)
    this
  }

  def delete: ListZipper[A] = {
    if (!curFocus.isEmpty) {
      if (right.nonEmpty) {
        curFocus = Some(right.head)
        right = right.tail
      } else if (left.nonEmpty) {
        curFocus = Some(left.last)
        left = left.take(left.size - 1)
        right = Nil
      } else {
        left = Nil
        right = Nil
        curFocus = None
      }
    }
    this
  }

  def mergeLeft(fn: (A, A) => A): ListZipper[A] = {
    if (prev.isDefined && curFocus.isDefined) {
      curFocus = Some(fn(prev.get, curFocus.get))
      left = left.take(left.size - 1)
    }
    this
  }

  def mergeRight(fn: (A, A) => A): ListZipper[A] = {
    if (next.isDefined && curFocus.isDefined) {
      curFocus = Some(fn(curFocus.get, next.get))
      right = right.tail
    }
    this
  }

  def next: Option[A] = right match {
    case Nil => None
    case _   => Some(right.head)
  }

  def prev: Option[A] = left match {
    case Nil => None
    case _   => Some(left.last)
  }
}
