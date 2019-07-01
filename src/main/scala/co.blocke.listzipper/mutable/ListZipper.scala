package co.blocke.listzipper.mutable

import scala.reflect.runtime.universe._

object ListZipper {
  def apply[A](initial: Seq[A]): ListZipper[A] =
    if (initial.isEmpty)
      ListZipper(Nil, None, Nil)
    else
      ListZipper(Nil, Some(initial.head), initial.tail.toList)
}

case class ListZipper[A](private val l: List[A], private val f: Option[A], private val r: List[A]) {

  private var left: List[A] = l
  private var _focus: Option[A] = f
  private var right: List[A] = r

  def focus: Option[A] = _focus
  def focusAs[T <: A](implicit tt: TypeTag[T]): Option[T] = _focus match {
    case Some(a) if isType[T](a) => Some(a.asInstanceOf[T])
    case _                       => None
  }

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

  def mergeLeftAs[T <: A](fn: (T, T) => T)(implicit tt: TypeTag[T]): ListZipper[A] = {
    if (prevAs[T].isDefined && _focus.isDefined && isType[T](_focus.get)) {
      _focus = Some(fn(prevAs[T].get, _focus.get.asInstanceOf[T]))
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

  def mergeRightAs[T <: A](fn: (T, T) => T)(implicit tt: TypeTag[T]): ListZipper[A] = {
    if (nextAs[T].isDefined && _focus.isDefined && isType[T](_focus.get)) {
      _focus = Some(fn(_focus.get.asInstanceOf[T], nextAs[T].get))
      right = right.tail
    }
    this
  }

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
