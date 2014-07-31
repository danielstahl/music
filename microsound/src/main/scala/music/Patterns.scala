package music

import music.Elide.Elide

trait PatternItem[+A] {
  def takeItem(): A
}

case class AtomItem[+A](atom: A) extends PatternItem[A] {
  def takeItem(): A = atom
}

object Patterns {
  def atom[A](atom: A): AtomItem[A] = AtomItem(atom)

  def constant[A, T <% PatternItem[A]](item: A): ConstantPattern[A, PatternItem[A]] = ConstantPattern(AtomItem(item))

  def cycle[A, T <% PatternItem[A]](items: T*): CyclePattern[A, T] =
    CyclePattern[A, T](items.toList)

  def palindrome[A, T <% PatternItem[A]](items: T*): PalindromePattern[A, T] =
    PalindromePattern[A, T](items.toList, Elide.NONE)

  def palindrome[A, T <% PatternItem[A]](elide: Elide, items: T*): PalindromePattern[A, T] =
    PalindromePattern[A, T](items.toList, elide)

  def line[A, T <% PatternItem[A]](items: T*): LinePattern[A, T] =
    LinePattern[A, T](items.toList)
}

abstract class Pattern[A, T <% PatternItem[A]] extends PatternItem[A] {
  var stream: Stream[A] = makeStream

  def makeStream: Stream[A]

  def takeItem(): A = {
    val item = stream.head
    stream = stream.tail
    item
  }
}

case class ConstantPattern[A, T <% PatternItem[A]](item: T) extends Pattern[A, T] {
  override def makeStream: Stream[A] = {
     def constantStream(curr: T): Stream[A] = {
       curr.takeItem() #:: constantStream(curr)
     }
    constantStream(item)
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
        case Nil => sys.error("The items is Nil")
      }
    }
    lineStream(items)
  }
}

case class CyclePattern[A, T <% PatternItem[A]](items: List[T]) extends AbstractCyclePattern[A, T] {
  def getItems: List[T] = items
}


object Elide extends Enumeration {
  type Elide = Value
  val NONE, BOTH, FIRST, LAST = Value
}

case class PalindromePattern[A, T <% PatternItem[A]](items: List[T], elide: Elide = Elide.NONE) extends AbstractCyclePattern[A, T] {
  private def endElide(xs: List[T], elide: Boolean) =
    if(elide) xs.drop(1) else xs

  private def startElide(xs: List[T], elide: Boolean) =
    if(elide) xs.dropRight(1) else xs

  private def reverse = elide match {
    case Elide.NONE => startElide(endElide(items.reverse, false), false)
    case Elide.BOTH => startElide(endElide(items.reverse, true), true)
    case Elide.FIRST => startElide(endElide(items.reverse, false), true)
    case Elide.LAST => startElide(endElide(items.reverse, true), false)
  }

  def getItems: List[T] = items ++ reverse
}