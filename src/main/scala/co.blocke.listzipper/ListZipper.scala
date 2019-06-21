package co.blocke.listzipper

object ListZipper {
  def apply[A](initial: Seq[A]): ListZipper[A] =
    if (initial.isEmpty)
      ListZipper(Nil, None, Nil)
    else
      ListZipper(Nil, Some(initial.head), initial.tail.toList)
}

case class ListZipper[A](left: List[A], focus: Option[A], right: List[A]) {

  def get: Option[A] = focus

  def isEmpty: Boolean = left.isEmpty && right.isEmpty && focus.isEmpty
  def nonEmpty: Boolean = left.nonEmpty || right.nonEmpty || focus.isDefined
  def crashedLeft: Boolean = left.isEmpty && focus.isEmpty
  def crashedRight: Boolean = right.isEmpty && focus.isEmpty

  def size: Int = left.size + right.size + { if (focus.isDefined) 1 else 0 }
  def index: Int = left match {
    case Nil if focus.isDefined => 0
    case Nil                    => -1 // off left edge
    case _ if focus.isEmpty     => -2 // off right edge
    case _                      => left.size
  }

  def toList: List[A] = left ++ { if (focus.isDefined) focus.get +: right else right }

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
    case Nil if focus.isDefined => ListZipper(Nil, None, this.focus.get +: right) // on the left edge...defocus
    case Nil                    => this // already off the left edge... do nothing
    case _ if focus.isEmpty     => ListZipper(left.take(left.length - 1), Some(left.last), Nil)
    case _                      => ListZipper(left.take(left.length - 1), Some(left.last), this.focus.get +: right)
  }

  def moveLeftWhile(fn: A => Boolean): ListZipper[A] = {
    var z = this
    while (z.focus.isDefined && fn(z.focus.get))
      z = z.moveLeft
    z
  }

  def moveRight: ListZipper[A] = right match {
    case Nil if focus.isDefined => ListZipper(left :+ this.focus.get, None, Nil) // on the right edge...defocus
    case Nil                    => this // already off the right edge... do nothing
    case _ if focus.isEmpty     => ListZipper(Nil, Some(right.head), right.tail)
    case _                      => ListZipper(left :+ this.focus.get, Some(right.head), right.tail)
  }

  def moveRightWhile(fn: A => Boolean): ListZipper[A] = {
    var z = this
    while (z.focus.isDefined && fn(z.focus.get))
      z = z.moveRight
    z
  }

  def modify(a: A): ListZipper[A] =
    if (isEmpty)
      ListZipper(Nil, Some(a), Nil)
    else if (focus.isDefined)
      this.copy(focus = Some(a))
    else
      this // nothing changed...nothing in focus

  def insertBefore(a: A): ListZipper[A] =
    if (focus.isDefined)
      ListZipper(left :+ a, focus, right)
    else
      ListZipper(left, Some(a), right)

  def insertAfter(a: A): ListZipper[A] =
    if (focus.isDefined)
      ListZipper(left, focus, a +: right)
    else
      ListZipper(left, Some(a), right)

  def delete: ListZipper[A] =
    if (focus.isEmpty)
      this
    else if (right.nonEmpty)
      ListZipper(left, Some(right.head), right.tail)
    else if (left.nonEmpty)
      ListZipper(left.take(left.size - 1), Some(left.last), Nil)
    else
      ListZipper(Nil, None, Nil)

  def mergeLeft(fn: (A, A) => A): ListZipper[A] =
    if (prev.isDefined && focus.isDefined)
      ListZipper(left.take(left.size - 1), Some(fn(prev.get, focus.get)), right)
    else
      this

  def mergeRight(fn: (A, A) => A): ListZipper[A] =
    if (next.isDefined && focus.isDefined)
      ListZipper(left, Some(fn(focus.get, next.get)), right.tail)
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
