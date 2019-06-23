package co.blocke.listzipper

import scala.reflect.runtime.universe._

object ListZipper {
  def apply[A](initial: Seq[A]): ListZipper[A] =
    if (initial.isEmpty)
      ListZipper(Nil, None, Nil)
    else
      ListZipper(Nil, Some(initial.head), initial.tail.toList)
}

case class ListZipper[A](left: List[A], _focus: Option[A], right: List[A]) {

  def focus: Option[A] = _focus

  @inline final def staticClass(fullName: String): ClassSymbol = scala.reflect.runtime.currentMirror.staticClass(fullName)
  @inline final def typeFromClassName(className: String): Type = staticClass(className).toType
  @inline final def isType[T](a: Any)(implicit tt: TypeTag[T]) = typeFromClassName(a.getClass.getName) == tt.tpe

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

  def mergeLeftAs[T <: A](fn: (T, T) => T)(implicit tt: TypeTag[T]): ListZipper[A] =
    if (prevAs[T].isDefined && _focus.isDefined && isType[T](_focus.get))
      ListZipper(left.take(left.size - 1), Some(fn(prevAs[T].get, _focus.get.asInstanceOf[T])), right)
    else
      this

  def mergeRight(fn: (A, A) => A): ListZipper[A] =
    if (next.isDefined && _focus.isDefined)
      ListZipper(left, Some(fn(_focus.get, next.get)), right.tail)
    else
      this

  def mergeRightAs[T <: A](fn: (T, T) => T)(implicit tt: TypeTag[T]): ListZipper[A] =
    if (nextAs[T].isDefined && _focus.isDefined && isType[T](_focus.get))
      ListZipper(left, Some(fn(_focus.get.asInstanceOf[T], nextAs[T].get)), right.tail)
    else
      this

  def next: Option[A] = right match {
    case Nil => None
    case _   => Some(right.head)
  }

  def nextAs[T <: A](implicit tt: TypeTag[T]): Option[T] =
    if (right.isEmpty)
      None
    else right.head match {
      case a if isType[T](a) => Some(a.asInstanceOf[T])
      case _                 => None
    }

  def prev: Option[A] = left match {
    case Nil => None
    case _   => Some(left.last)
  }

  def prevAs[T <: A](implicit tt: TypeTag[T]): Option[T] =
    if (left.isEmpty)
      None
    else left.last match {
      case a if isType[T](a) => Some(a.asInstanceOf[T])
      case _                 => None
    }
}
