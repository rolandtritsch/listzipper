package co.blocke.collection.immutable

object ListZipper {
  def apply[A](initial: Seq[A]): ListZipper[A] =
    if (initial.isEmpty)
      ListZipper(Nil, None, Nil)
    else
      ListZipper(Nil, Some(initial.head), initial.tail.toList)

  def empty[A]: ListZipper[A] = ListZipper(Nil,None,Nil)
}

case class ListZipper[A](left: List[A], _focus: Option[A], right: List[A]) {

  def focus: Option[A] = _focus

  def isEmpty: Boolean = left.isEmpty && right.isEmpty && _focus.isEmpty
  def nonEmpty: Boolean = left.nonEmpty || right.nonEmpty || _focus.isDefined
  def crashedLeft: Boolean = left.isEmpty && _focus.isEmpty
  def crashedRight: Boolean = right.isEmpty && _focus.isEmpty

  def size: Int = left.size + right.size + { if (_focus.isDefined) 1 else 0 }
  def index: Int = left match {
    case Nil if _focus.isDefined => 0
    case Nil                     => -1 // off left edge
    case _ if _focus.isEmpty     => -2 // off right edge
    case _                       => left.size
  }

  def map[B](f: (A) => B): ListZipper[B] = 
    val ml = left.map( a => f(a) )
    val mf = _focus.map( a => f(a) )
    val mr = right.map( a => f(a) )
    ListZipper(ml,mf,mr)

  def flagMap[B](f: (A) => IterableOnce[B]): ListZipper[B] =
    val ml = left.flatMap( a => f(a) )
    val mf = _focus.flatMap( a => f(a).iterator.to(List).headOption )
    val mr = right.flatMap( a => f(a) )
    ListZipper(ml,mf,mr)

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

  def toList: List[A] = left ++ { if (_focus.isDefined) _focus.get +: right else right }

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
    case Nil if _focus.isDefined => ListZipper(Nil, None, this._focus.get +: right) // on the left edge...de_focus
    case Nil                     => this // already off the left edge... do nothing
    case _ if _focus.isEmpty     => ListZipper(left.take(left.length - 1), Some(left.last), Nil)
    case _                       => ListZipper(left.take(left.length - 1), Some(left.last), this._focus.get +: right)
  }

  def moveLeftWhile(fn: A => Boolean): ListZipper[A] = {
    var z = this
    while (z._focus.isDefined && fn(z._focus.get))
      z = z.moveLeft
    z
  }

  def moveRight: ListZipper[A] = right match {
    case Nil if _focus.isDefined => ListZipper(left :+ this._focus.get, None, Nil) // on the right edge...de_focus
    case Nil                     => this // already off the right edge... do nothing
    case _ if _focus.isEmpty     => ListZipper(Nil, Some(right.head), right.tail)
    case _                       => ListZipper(left :+ this._focus.get, Some(right.head), right.tail)
  }

  def moveRightWhile(fn: A => Boolean): ListZipper[A] = {
    var z = this
    while (z._focus.isDefined && fn(z._focus.get))
      z = z.moveRight
    z
  }

  def modify(a: A): ListZipper[A] =
    if (isEmpty)
      ListZipper(Nil, Some(a), Nil)
    else if (_focus.isDefined)
      this.copy(_focus = Some(a))
    else
      this // nothing changed...nothing in _focus

  def insertBefore(a: A): ListZipper[A] =
    if (_focus.isDefined)
      ListZipper(left :+ a, _focus, right)
    else
      ListZipper(left, Some(a), right)

  def insertAfter(a: A): ListZipper[A] =
    if (_focus.isDefined)
      ListZipper(left, _focus, a +: right)
    else
      ListZipper(left, Some(a), right)

  def delete: ListZipper[A] =
    if (_focus.isEmpty)
      this
    else if (right.nonEmpty)
      ListZipper(left, Some(right.head), right.tail)
    else if (left.nonEmpty)
      ListZipper(left.take(left.size - 1), Some(left.last), Nil)
    else
      ListZipper(Nil, None, Nil)

  def mergeLeft(fn: (A, A) => A): ListZipper[A] =
    if (prev.isDefined && _focus.isDefined)
      ListZipper(left.take(left.size - 1), Some(fn(prev.get, _focus.get)), right)
    else
      this

  def mergeRight(fn: (A, A) => A): ListZipper[A] =
    if (next.isDefined && _focus.isDefined)
      ListZipper(left, Some(fn(_focus.get, next.get)), right.tail)
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
