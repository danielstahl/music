package music

import org.scalatest.FlatSpec
import music.TimeItem._
import music.Patterns._

/**
 * Testclass for Pulse
 */
class ScaledTimeSpec extends FlatSpec {
  behavior of "TimeAtom"

  it should "scale to one item equal to its total time" in {
    val atom = timeAtom
    assert(atom.buildRelativeTime(100) === List(100))
  }

  it should "scaleAndSum to one TimeItem with duration of total time" in {
    val atom = timeAtom
    assert(atom.build(0, 10) === List(TimeItem(0, 10)))
  }

  behavior of "PulseScaledTime"

  it should "return  n number of steps items with each duration totalDuration / step" in {
    val pulse = pulseScaledTime(4, timeAtom)
    assert(pulse.buildRelativeTime(100) === List(25, 25, 25, 25))
  }

  behavior of "RelativeScaledTime"

  it should "scale relative" in {
    val relativeTime = relativeScaledTime((1, timeAtom), (3, timeAtom))
    assert(relativeTime.buildRelativeTime(100) === List(25, 75))
  }

  behavior of "PatternScaledTime"

  it should "the scaled time be returned from a pattern" in {
     val patternTime = patternScaledTime(cycle(atom(timeAtom)))
     assert(patternTime.buildRelativeTime(100) === List(100))
  }

  behavior of "All scaled time"

  it should "be possible to nest" in {
    val time = relativeScaledTime((1, timeAtom), (3, pulseScaledTime(3, timeAtom)))
    assert(time.buildRelativeTime(100) === List(25, 25, 25, 25))
  }

  behavior of "PulseTransform"

  it should "repeat the timeitems" in {
    val transformer = PulseTransformer(2)
    val originalTime = List(TimeItem(0, 25), TimeItem(25, 25))
    val resultTime = List(TimeItem(0, 25), TimeItem(25, 25), TimeItem(50, 25), TimeItem(75, 25), TimeItem(100, 25), TimeItem(125, 25))
    assert(transformer.transform(originalTime) === resultTime)
  }

  it should "respect the startTime" in {
    val transformer = PulseTransformer(1)
    val originalTime = List(TimeItem(1, 25), TimeItem(26, 25))
    val resultTime = List(TimeItem(1, 25), TimeItem(26, 25), TimeItem(51, 25), TimeItem(76, 25))
    assert(transformer.transform(originalTime) === resultTime)
  }

  behavior of "ScaleTransform"

  it should "scale the timeitems by a factor" in {
    val transformer = ScaleTransformer(0.5f)
    val originalTime = List(TimeItem(0, 50), TimeItem(50, 50))
    val resultTime = List(TimeItem(0, 25), TimeItem(25, 25))
    assert(transformer.transform(originalTime) === resultTime)
  }

  it should "respect the startTime in the ScaleTransform" in {
    val transformer = ScaleTransformer(0.5f)
    val originalTime = List(TimeItem(25, 50), TimeItem(75, 50))
    val resultTime = List(TimeItem(25, 25), TimeItem(50, 25))
    assert(transformer.transform(originalTime) === resultTime)
  }

  behavior of "ChainedTimeItemTransformer"

  it should "chain transformers, applying the one after the other" in {
    val transformers = ChainedTimeItemTransformer(ScaleTransformer(0.5f), ScaleTransformer(0.5f))
    val originalTime = List(TimeItem(0, 100), TimeItem(100, 100))
    val resultTime = List(TimeItem(0, 25), TimeItem(25, 25))
    assert(transformers.transform(originalTime) === resultTime)
  }

  behavior of "PatternTimeItemTransformer"

  it should "get its transformer from a pattern" in {
    val transformers = PatternTimeItemTransformer(line(atom(ScaleTransformer(0.5f)), atom(ScaleTransformer(2.0f))))
    val originalTime = List(TimeItem(0, 100), TimeItem(100, 100))
    assert(transformers.transform(originalTime) === List(TimeItem(0, 50), TimeItem(50, 50)))
    assert(transformers.transform(originalTime) === List(TimeItem(0, 200), TimeItem(200, 200)))
  }
}
