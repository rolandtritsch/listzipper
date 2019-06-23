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
  private var focus: Option[A] = f
  private var right: List[A] = r

  def get: Option[A] = focus

  @inline final def staticClass(fullName: String): ClassSymbol = scala.reflect.runtime.currentMirror.staticClass(fullName)
  @inline final def typeFromClassName(className: String): Type = staticClass(className).toType

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
    if (r.isEmpty) {
      left = l
      focus = None
      right = Nil
    } else if (i >= 0) {
      left = l
      focus = Some(r.head)
      right = r.tail
    } else {
      left = l
      focus = None
      right = r
    }
    this
  }

  def moveLeft: ListZipper[A] = left match {
    case Nil if focus.isDefined =>
      left = Nil
      right = focus.get +: right
      focus = None
      this
    case Nil => this // already off the left edge... do nothing
    case _ if focus.isEmpty =>
      focus = Some(left.last)
      left = left.take(left.length - 1)
      right = Nil
      this
    case _ =>
      right = focus.get +: right
      focus = Some(left.last)
      left = left.take(left.length - 1)
      this
  }

  def moveLeftWhile(fn: A => Boolean): ListZipper[A] = {
    while (focus.isDefined && fn(focus.get))
      moveLeft
    this
  }

  def moveRight: ListZipper[A] = right match {
    case Nil if focus.isDefined =>
      left = left :+ this.focus.get
      focus = None
      right = Nil
      this
    case Nil => this // already off the right edge... do nothing
    case _ if focus.isEmpty =>
      left = Nil
      focus = Some(right.head)
      right = right.tail
      this
    case _ =>
      left = left :+ this.focus.get
      focus = Some(right.head)
      right = right.tail
      this
  }

  def moveRightWhile(fn: A => Boolean): ListZipper[A] = {
    while (focus.isDefined && fn(focus.get))
      moveRight
    this
  }

  def modify(a: A): ListZipper[A] = {
    if (isEmpty || focus.isDefined)
      focus = Some(a)
    this // nothing changed...nothing in focus
  }

  def insertBefore(a: A): ListZipper[A] = {
    if (focus.isDefined)
      left = left :+ a
    else
      focus = Some(a)
    this
  }

  def insertAfter(a: A): ListZipper[A] = {
    if (focus.isDefined)
      right = a +: right
    else
      focus = Some(a)
    this
  }

  def delete: ListZipper[A] = {
    if (!focus.isEmpty) {
      if (right.nonEmpty) {
        focus = Some(right.head)
        right = right.tail
      } else if (left.nonEmpty) {
        focus = Some(left.last)
        left = left.take(left.size - 1)
        right = Nil
      } else {
        left = Nil
        right = Nil
        focus = None
      }
    }
    this
  }

  def mergeLeft(fn: (A, A) => A): ListZipper[A] = {
    if (prev.isDefined && focus.isDefined) {
      focus = Some(fn(prev.get, focus.get))
      left = left.take(left.size - 1)
    }
    this
  }

  def mergeRight(fn: (A, A) => A): ListZipper[A] = {
    if (next.isDefined && focus.isDefined) {
      focus = Some(fn(focus.get, next.get))
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
      case a if typeFromClassName(a.getClass.getName) == tt.tpe => Some(a.asInstanceOf[T])
      case _ => None
    }

  def prev: Option[A] = left match {
    case Nil => None
    case _   => Some(left.last)
  }

  def prevAs[T <: A](implicit tt: TypeTag[T]): Option[A] =
    if (left.isEmpty)
      None
    else left.last match {
      case a if typeFromClassName(a.getClass.getName) == tt.tpe => Some(a.asInstanceOf[T])
      case _ => None
    }
}
