package music

import scala.util.Either

trait PatternItem[A] {
  def takeItem(): A
}

case class AtomItem[A](atom: A) extends PatternItem[A] {
  def takeItem(): A = atom
}

object Patterns {

  def atom[A](atom: A) = AtomItem(atom)

  def cycle[A, T <% PatternItem[A]](items: T*): CyclePattern[A, T] =
    CyclePattern[A, T](items.toList)

  def palindrome[A, T <% PatternItem[A]](items: T*): PalindromePattern[A, T] =
    PalindromePattern[A, T](items.toList, Left(true))

  def palindrome[A, T <% PatternItem[A]](elide: Either[Boolean, (Boolean, Boolean)], items: T*): PalindromePattern[A, T] =
    PalindromePattern[A, T](items.toList, Left(true))

  def line[A, T <% PatternItem[A]](items: T*): LinePattern[A, T] =
    LinePattern[A, T](items.toList)
}

abstract class Pattern[A, T <% PatternItem[A]] extends PatternItem[A] {
  var stream: Stream[A] = makeStream

  def makeStream: Stream[A]

  def takeItem(): A = {
    var item = stream.head
    stream = stream.tail
    item
  }
}

abstract class AbstractCyclePattern[A, T <% PatternItem[A]] extends Pattern[A, T] {
  def getItems: List[T]

  override def makeStream: Stream[A] = {
    def cycleStream(rest: List[T]): Stream[A] = {
      rest match {
        case Nil => cycleStream(getItems)
        case x :: xs => x.takeItem #:: cycleStream(xs)
      }
    }
    cycleStream(getItems)
  }
}

case class LinePattern[A, T <% PatternItem[A]](items: List[T]) extends Pattern[A, T] {
  override def makeStream: Stream[A] = {
    def lineStream(curr: List[T]): Stream[A] = {
      curr match {
        case x :: Nil =>
          x.takeItem #:: lineStream(curr)
        case x :: xs =>
          x.takeItem #:: lineStream(xs)
      }
    }
    lineStream(items)
  }
}

case class CyclePattern[A, T <% PatternItem[A]](items: List[T]) extends AbstractCyclePattern[A, T] {
  def getItems: List[T] = items
}

case class PalindromePattern[A, T <% PatternItem[A]](items: List[T], elide: Either[Boolean, (Boolean, Boolean)] = Left(true)) extends AbstractCyclePattern[A, T] {
  private def endElide(xs: List[T], elide: Boolean) =
    if(!elide) xs.drop(1) else xs

  private def startElide(xs: List[T], elide: Boolean) =
    if(!elide) xs.dropRight(1) else xs

  private def reverse = elide match {
    case Left(e) => endElide(items.reverse, e)
    case Right((es, ee)) =>
      startElide(endElide(items.reverse, ee), es)
  }

  def getItems: List[T] = items ++ reverse
}