package music

import org.scalatest.FlatSpec
import music.Patterns._

/**
 * Test-class for patterns
 */
class PatternsSpec extends FlatSpec {

  behavior of "A atom"

  it should "return its value" in {
    val value = "value"
    val atomItem = atom(value)
    assert(atomItem.takeItem() === value)
  }

  behavior of "Constant pattern"

  it should "return the same value" in {
    val value = "value"
    val constanctPattern = constant(value)
    assert(constanctPattern.takeItem() === value)
    assert(constanctPattern.takeItem() === value)
    assert(constanctPattern.takeItem() === value)
  }

  behavior of "A line pattern"

  it should "get stuck on its last element" in {
    val expectedItems = List(1, 2, 3, 3, 3, 3)
    val linePattern = line(atom(1), atom(2), atom(3))
    assert(linePattern.stream.take(6).toList === expectedItems)
  }

  behavior of "A cycle pattern"

  it should "repeat the items" in {
    val expectedItems = List(1, 2, 3, 1, 2, 3)
    val linePattern = cycle(atom(1), atom(2), atom(3))
    assert(linePattern.stream.take(6).toList === expectedItems)
  }

  behavior of "A palindrome"

  it should "default reverse itself with repeat (elide NONE)" in {
    val expectedItems = List(1, 2, 3, 3, 2, 1, 1)
    val palindromePattern = palindrome(atom(1), atom(2), atom(3))
    assert(palindromePattern.stream.take(7).toList === expectedItems)
  }

  it should "reverse itself with repeat when elide is NONE" in {
    val expectedItems = List(1, 2, 3, 3, 2, 1, 1)
    val palindromePattern = palindrome(Elide.NONE, atom(1), atom(2), atom(3))
    assert(palindromePattern.stream.take(7).toList === expectedItems)
  }

  it should "reverse itself without repeat when elide is BOTH" in {
    val expectedItems = List(1, 2, 3, 2, 1, 2)
    val palindromePattern = palindrome(Elide.BOTH, atom(1), atom(2), atom(3))
    assert(palindromePattern.stream.take(6).toList === expectedItems)
  }

  it should "reverse itself without repeat at end when elide is LAST" in {
    val expectedItems = List(1, 2, 3, 2, 1, 1)
    val palindromePattern = palindrome(Elide.LAST, atom(1), atom(2), atom(3))
    assert(palindromePattern.stream.take(6).toList === expectedItems)
  }

  it should "reverse itself without repeat at start when elide is FIRST" in {
    val expectedItems = List(1, 2, 3, 3, 2, 1, 2)
    val palindromePattern = palindrome(Elide.FIRST, atom(1), atom(2), atom(3))
    assert(palindromePattern.stream.take(7).toList === expectedItems)
  }

  behavior of "All patterns"

  it should "be possible to nest" in {
    val expectedItems = List(1, 2, 5, 6, 5, 6)
    val nestedPattern = line(atom(1), atom(2), cycle(atom(5), atom(6)))
  }
}
