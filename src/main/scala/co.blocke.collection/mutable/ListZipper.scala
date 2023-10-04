package co.blocke.collection.mutable

object ListZipper {
  def apply[A](initial: Seq[A]): ListZipper[A] =
    if (initial.isEmpty)
      ListZipper(Nil, None, Nil)
    else
      ListZipper(Nil, Some(initial.head), initial.tail.toList)

  def empty[A]: ListZipper[A] = ListZipper(Nil,None,Nil)
}

case class ListZipper[A](private var left: List[A], private var _focus: Option[A], private var right: List[A]) {

  // private var left: List[A] = l
  // private var _focus: Option[A] = f
  // private var right: List[A] = r

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

  def first: ListZipper[A] = {
    if (!isEmpty) {
      val all = this.toList
      left = Nil
      _focus = Some(all.head)
      right = all.tail
    }
    this
  }
  def last: ListZipper[A] = {
    if (!isEmpty) {
      val all = this.toList
      left = all.take(all.size - 1)
      _focus = Some(all.last)
      right = Nil
    }
    this
  }

  def toList: List[A] = left ++ { if (_focus.isDefined) _focus.get +: right else right }

  def moveTo(i: Int): ListZipper[A] = {
    val asList = toList
    val (l, r) = asList.splitAt(i)
    if (r.isEmpty) {
      left = l
      _focus = None
      right = Nil
    } else if (i >= 0) {
      left = l
      _focus = Some(r.head)
      right = r.tail
    } else {
      left = l
      _focus = None
      right = r
    }
    this
  }

  def moveLeft: ListZipper[A] = left match {
    case Nil if _focus.isDefined =>
      left = Nil
      right = _focus.get +: right
      _focus = None
      this
    case Nil => this // already off the left edge... do nothing
    case _ if _focus.isEmpty =>
      _focus = Some(left.last)
      left = left.take(left.length - 1)
      right = Nil
      this
    case _ =>
      right = _focus.get +: right
      _focus = Some(left.last)
      left = left.take(left.length - 1)
      this
  }

  def moveLeftWhile(fn: A => Boolean): ListZipper[A] = {
    while (_focus.isDefined && fn(_focus.get))
      moveLeft
    this
  }

  def moveRight: ListZipper[A] = right match {
    case Nil if _focus.isDefined =>
      left = left :+ this._focus.get
      _focus = None
      right = Nil
      this
    case Nil => this // already off the right edge... do nothing
    case _ if _focus.isEmpty =>
      left = Nil
      _focus = Some(right.head)
      right = right.tail
      this
    case _ =>
      left = left :+ this._focus.get
      _focus = Some(right.head)
      right = right.tail
      this
  }

  def moveRightWhile(fn: A => Boolean): ListZipper[A] = {
    while (_focus.isDefined && fn(_focus.get))
      moveRight
    this
  }

  def modify(a: A): ListZipper[A] = {
    if (isEmpty || _focus.isDefined)
      _focus = Some(a)
    this // nothing changed...nothing in _focus
  }

  def insertBefore(a: A): ListZipper[A] = {
    if (_focus.isDefined)
      left = left :+ a
    else
      _focus = Some(a)
    this
  }

  def insertAfter(a: A): ListZipper[A] = {
    if (_focus.isDefined)
      right = a +: right
    else
      _focus = Some(a)
    this
  }

  def delete: ListZipper[A] = {
    if (!_focus.isEmpty) {
      if (right.nonEmpty) {
        _focus = Some(right.head)
        right = right.tail
      } else if (left.nonEmpty) {
        _focus = Some(left.last)
        left = left.take(left.size - 1)
        right = Nil
      } else {
        left = Nil
        right = Nil
        _focus = None
      }
    }
    this
  }

  def mergeLeft(fn: (A, A) => A): ListZipper[A] = {
    if (prev.isDefined && _focus.isDefined) {
      _focus = Some(fn(prev.get, _focus.get))
      left = left.take(left.size - 1)
    }
    this
  }

  def mergeRight(fn: (A, A) => A): ListZipper[A] = {
    if (next.isDefined && _focus.isDefined) {
      _focus = Some(fn(_focus.get, next.get))
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
