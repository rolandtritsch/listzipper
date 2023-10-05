package co.blocke.collection.immutable

object ListZipper {
  def apply[A](initial: Seq[A]): ListZipper[A] =
    if (initial.isEmpty)
      ListZipper(Nil, None, Nil)
    else
      ListZipper(Nil, Some(initial.head), initial.tail.toList)

  def empty[A]: ListZipper[A] = ListZipper(Nil, None, Nil)
}

case class ListZipper[A](left: List[A], curFocus: Option[A], right: List[A]) {

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

  def first: ListZipper[A] =
    if (isEmpty)
      this
    else {
      val all = this.toList
      ListZipper(Nil, Some(all.head), all.tail)
    }
  def last: ListZipper[A] =
    if (isEmpty)
      this
    else {
      val all = this.toList
      ListZipper(all.take(all.size - 1), Some(all.last), Nil)
    }

  def toList: List[A] = left ++ { if (curFocus.isDefined) curFocus.get +: right else right }

  def moveTo(i: Int): ListZipper[A] = {
    val asList = toList
    val (l, r) = asList.splitAt(i)
    if (r.isEmpty)
      ListZipper(l, None, Nil)
    else if (i >= 0)
      ListZipper(l, Some(r.head), r.tail)
    else
      ListZipper(l, None, r)
  }

  def moveLeft: ListZipper[A] = left match {
    case Nil if curFocus.isDefined => ListZipper(Nil, None, this.curFocus.get +: right) // on the left edge...decurFocus
    case Nil                       => this // already off the left edge... do nothing
    case _ if curFocus.isEmpty     => ListZipper(left.take(left.length - 1), Some(left.last), Nil)
    case _                         => ListZipper(left.take(left.length - 1), Some(left.last), this.curFocus.get +: right)
  }

  def moveLeftWhile(fn: A => Boolean): ListZipper[A] = {
    var z = this
    while (z.curFocus.isDefined && fn(z.curFocus.get))
      z = z.moveLeft
    z
  }

  def moveRight: ListZipper[A] = right match {
    case Nil if curFocus.isDefined => ListZipper(left :+ this.curFocus.get, None, Nil) // on the right edge...decurFocus
    case Nil                       => this // already off the right edge... do nothing
    case _ if curFocus.isEmpty     => ListZipper(Nil, Some(right.head), right.tail)
    case _                         => ListZipper(left :+ this.curFocus.get, Some(right.head), right.tail)
  }

  def moveRightWhile(fn: A => Boolean): ListZipper[A] = {
    var z = this
    while (z.curFocus.isDefined && fn(z.curFocus.get))
      z = z.moveRight
    z
  }

  def modify(a: A): ListZipper[A] =
    if (isEmpty)
      ListZipper(Nil, Some(a), Nil)
    else if (curFocus.isDefined)
      this.copy(curFocus = Some(a))
    else
      this // nothing changed...nothing in curFocus

  def insertBefore(a: A): ListZipper[A] =
    if (curFocus.isDefined)
      ListZipper(left :+ a, curFocus, right)
    else
      ListZipper(left, Some(a), right)

  def insertAfter(a: A): ListZipper[A] =
    if (curFocus.isDefined)
      ListZipper(left, curFocus, a +: right)
    else
      ListZipper(left, Some(a), right)

  def delete: ListZipper[A] =
    if (curFocus.isEmpty)
      this
    else if (right.nonEmpty)
      ListZipper(left, Some(right.head), right.tail)
    else if (left.nonEmpty)
      ListZipper(left.take(left.size - 1), Some(left.last), Nil)
    else
      ListZipper(Nil, None, Nil)

  def mergeLeft(fn: (A, A) => A): ListZipper[A] =
    if (prev.isDefined && curFocus.isDefined)
      ListZipper(left.take(left.size - 1), Some(fn(prev.get, curFocus.get)), right)
    else
      this

  def mergeRight(fn: (A, A) => A): ListZipper[A] =
    if (next.isDefined && curFocus.isDefined)
      ListZipper(left, Some(fn(curFocus.get, next.get)), right.tail)
    else
      this

  def next: Option[A] = right match {
    case Nil => None
    case _   => Some(right.head)
  }

  def prev: Option[A] = left match {
    case Nil => None
    case _   => Some(left.last)
  }

}
