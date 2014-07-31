package music

import Harmony._
import java.awt.Graphics2D

import music.Common._

/**
 * harm(HARMON, 0, 3, 4, 5, 6, 7)
 * 6.875, 8.59375, 10.3125, 12.03125, 13.75
 * harm(PHI, 0, 1, 2, 3, 4, 5)
 * 4.499746, 7.2807417, 10.061738, 12.842733, 15.62373
 * harm(INVERTED_PHI, 0, 5, 6, 7, 8, 9)
 * 7.0299797, 8.092226, 9.154471, 10.216718, 11.278963
 */

sealed case class AttackType(value: Float)

object SHARP_INVPHI extends AttackType(1 - invPhi)

object HALF extends AttackType(0.5f)

object SOFT_INVPHI extends AttackType(invPhi)

sealed case class PulseChord(values: Seq[Float])

object HARM_CHORD extends PulseChord(harm(SpectrumName.HARMON, 0, 3, 4, 5, 6, 7))
object PHI_CHORD extends PulseChord(harm(SpectrumName.PHI, 0, 1, 2, 3, 4, 5))
object IPHI_CHORD extends PulseChord(harm(SpectrumName.INVERTED_PHI, 0, 5, 6, 7, 8, 9))

object SHORT_HARM_CHORD extends PulseChord(harm(SpectrumName.HARMON, 0, 0, 1, 2))
object SHORT_PHI_CHORD extends PulseChord(harm(SpectrumName.PHI, 0, 0, 1, 2))
object SHORT_IPHI_CHORD extends PulseChord(harm(SpectrumName.INVERTED_PHI, 0, 0, 1, 2, 3, 4))


object Pulse {

  import NoteName._

  def simplePulse(bases: Seq[PulseTime], notes: Seq[LongNote], pans: Seq[PanArgument], parts: Seq[PulseParts]): SimplePulse = {
    SimplePulse(bases, notes, pans, parts)
  }

  def base(chord: PulseChord, bases: (Int, Int, AttackType)*): Seq[PulseTime] = {
    bases.map {
      base => PulseTime(chord.values(base._1), TimeArgument(chord.values(base._2), base._3.value))
    }
  }

  def note(names: NoteName*): Seq[LongNote] = {
    names.map {
      noteName => LongNote(noteName)
    }
  }

  def pan(pans: (Float, Float)*): Seq[PanArgument] = {
    pans.map {
      pan => PanArgument(pan._1, pan._2)
    }
  }

  def parts(parts: PulseParts*): Seq[PulseParts] = {
    parts
  }

  def p(parts: (Int, PartPattern)*): PulseParts = PulseParts(
    parts.map {
      part => (part._1, part._2)
    })

  def parallelPulse(pulses: Pulse*): ParallelPulse = {
    ParallelPulse(pulses)
  }

  def sequencialPulse(pulses: Pulse*): SequencialPulse = {
    SequencialPulse(pulses)
  }

  private val pulse = pulse2
  // noise2 low, noise3 middle, noise1 high
  // 2 2 SHARP_INVPHI, 1 1 SHARP_INVPHI

  private val pulse2: Pulse =
    parallelPulse(                  //13.75
      simplePulse(base(HARM_CHORD, (4, 4, SOFT_INVPHI), (4, 4, SOFT_INVPHI), (4, 4, SOFT_INVPHI), (4, 4, SOFT_INVPHI), (4, 4, SOFT_INVPHI), (4, 4, SOFT_INVPHI), (4, 4, SOFT_INVPHI)),
        note(noise2, noise2, noise2, noise2, noise2, noise2, noise2),
        pan((-3f, -2f), (-2f, -1f), (-1f, 0f), (0f, -1), (-1f, -2), (-2f, -3f), (-3f, -3f)),
        parts()),                  //12.03125
      simplePulse(base(HARM_CHORD, (3, 3, HALF), (3, 3, HALF), (3, 3, HALF), (3, 3, HALF), (3, 3, HALF), (3, 3, HALF), (3, 3, HALF)),
        note(noise3, noise3, noise3, noise3, noise3, noise3, noise3),
        pan((3f, 2f), (2f, 1f), (1f, 0f), (0f, 1), (1f, 2), (2f, 3f), (3f, 3f)),
        parts()))


  private val pulse1: Pulse =
    sequencialPulse(
      simplePulse(base(HARM_CHORD, (2, 2, SOFT_INVPHI), (2, 2, SHARP_INVPHI), (2, 2, SOFT_INVPHI)),
        note(noise1, noise4, noise1),
        pan((-1f, -0.6f), (0.5f, -0.2f), (0.2f, -0.5f)),
        parts(p((5, SHARP_PATTERN1), (3, SHARP_PATTERN1), (8, SHARP_PATTERN1)),
          p((5, INV_SHARP_PATTERN1), (3, INV_SHARP_PATTERN1)),
          p((8, SHARP_PATTERN1), (3, SHARP_PATTERN1), (5, SHARP_PATTERN1)))),
      parallelPulse(
        simplePulse(base(HARM_CHORD, (1, 1, SHARP_INVPHI), (0, 0, SHARP_INVPHI), (1, 1, SHARP_INVPHI), (0, 0, SHARP_INVPHI), (0, 0, SHARP_INVPHI)),
          note(noise1, noise1, noise1, noise1, noise1),
          pan((0, 0.2f), (0.2f, 0.4f), (0.4f, 0.6f), (0.6f, 0.8f), (0.8f, 1f)),
          parts(p((5, SOFT_PATTERN1), (3, SOFT_PATTERN1), (8, SOFT_PATTERN1)),
            p((3, INV_SOFT_PATTERN1), (3, INV_SOFT_PATTERN1), (8, INV_SOFT_PATTERN1)),
            p((5, SOFT_PATTERN1), (5, SOFT_PATTERN1), (3, SOFT_PATTERN1)),
            p((3, INV_SOFT_PATTERN1), (8, INV_SOFT_PATTERN1), (3, INV_SOFT_PATTERN1)),
            p((5, SOFT_PATTERN1), (3, SOFT_PATTERN1), (3, SOFT_PATTERN1)))),
        simplePulse(base(HARM_CHORD, (0, 0, SOFT_INVPHI), (0, 0, SOFT_INVPHI), (1, 1, SOFT_INVPHI), (0, 0, SOFT_INVPHI), (1, 1, SOFT_INVPHI)),
          note(noise4, noise4, noise4, noise4, noise4),
          pan((0, -0.2f), (-0.2f, -0.4f), (-0.4f, -0.6f), (-0.6f, -0.8f), (-0.8f, -1)),
          parts(p((1, SHARPER_PATTERN1)), p((1, SHARPER_PATTERN1)), p((1, SHARPER_PATTERN1)), p((1, SHARPER_PATTERN1)), p((1, SHARPER_PATTERN1))))),
      simplePulse(base(HARM_CHORD, (1, 1, SHARP_INVPHI), (1, 1, SOFT_INVPHI), (1, 1, SHARP_INVPHI)),
        note(noise4, noise1, noise4),
        pan((1f, 0.6f), (-0.5f, 0.2f), (-0.2f, 0.5f)),
        parts(p((1, SHARP_PATTERN1)), p((1, SHARPER_PATTERN1)), p((1, INV_SOFT_PATTERN1)))))

  def apply(): Pulse = pulse2
}

case class PulseTime(delta: Float, time: TimeArgument)

trait Pulse extends Plottable with Playable {

  def play()(implicit player: MusicPlayer) = {
    internalPlay(0, 0)(player, Layers(0))
  }

  def internalPlay(absoluteTime: Float, track: Int)(implicit player: MusicPlayer, layers: Layers)

  def plot(g: Graphics2D) {
    internalPlot(0, 0, g)
  }

  def internalPlot(absolutTime: Float, track: Int, g: Graphics2D)

  def length: Float

  def tracks: Int

  def format(theValue: Number): String = {
    "%4.2f" format theValue
  }
}

case class SimplePulse(bases: Seq[PulseTime], notes: Seq[LongNote], pans: Seq[PanArgument], pulseParts: Seq[PulseParts]) extends Pulse {

  import GroupName._

  def internalPlay(absoluteTime: Float, track: Int)(implicit player: MusicPlayer, layers: Layers) = {
    internalPlay1(absoluteTime, track)
  }

  def internalPlay2(absoluteTime: Float, track: Int)(implicit player: MusicPlayer, layers: Layers) = {
    val baseOutput = OutbusArgument(16 + (track * 2)).arguments
    val patternInput = InbusArgument(16 + (track * 2)).arguments
    val patternOutput = OutbusArgument(0).arguments

    val baseSourceArgs = BaseArgument(targetNodeId = layers.getGroup(track, SOURCE)).arguments
    val baseGrainArgs = BaseArgument(targetNodeId = layers.getGroup(track, GRAIN)).arguments
    val baseAmp = AmpArgument(0.9f).arguments

    var tempTime = absoluteTime
    val noiseGrain = NoiseGrain2()
    ((bases, notes, pans).zipped.toSeq, pulseParts).zipped.foreach {
      case ((baseValue, noteValue, panValue), pulsePart) =>
        player.sendNew(noteValue.instrument.arguments ++
          baseSourceArgs ++
          baseValue.time.arguments ++
          baseAmp ++
          panValue.arguments ++
          baseOutput ++
          noteValue.args.flatMap(_.arguments), (tempTime * 1000).round.toLong)

        pulsePart.scaleAndSum(tempTime, baseValue.time.dur).foreach {
          case (start, dur, pattern) => {
            pattern.scaleAndSum(start, dur).foreach {
              case (pstart, pdur, amp, (attackType, attack)) => {
                player.sendNew(noiseGrain.arguments ++
                  baseGrainArgs ++
                  patternInput ++
                  patternOutput ++
                  AttackArgument2(attackType).arguments ++
                  TimeArgument(pdur, attack).arguments ++
                  AmpArgument(amp).arguments,
                  (pstart * 1000).round.toLong)
              }
            }
          }
        }
        tempTime = tempTime + baseValue.delta
    }

  }

  def internalPlay1(absoluteTime: Float, track: Int)(implicit player: MusicPlayer) = {
    val defaultBase: BaseArgument = BaseArgument()
    var tempTime = absoluteTime
    (bases, notes, pans).zipped.foreach {
      (baseValue, noteValue, panValue) =>
        player.sendNew(noteValue.instrument.arguments ++
          defaultBase.arguments ++
          baseValue.time.arguments ++
          panValue.arguments ++
          noteValue.args.flatMap(_.arguments), (tempTime * 1000).round.toLong)


        tempTime = tempTime + baseValue.delta
    }
  }

  def xpos(xtime: Float): Int = (10 + (xtime * 10).round).toInt


  def internalPlot(absolutTime: Float, track: Int, g: Graphics2D): Unit = internalPlot2(absolutTime, track, g)

  def internalPlot2(absoluteTime: Float, track: Int, g: Graphics2D) {
    val ypos = 200 - (track * 30)
    val ypulsepos = 300 - (track * 30)
    val ypattpos = 400 - (track * 30)
    var tempTime = absoluteTime
    (bases, pulseParts).zipped.foreach {
      (baseValue, pulsePart) =>
        val x = xpos(tempTime)
        g.drawLine(x, ypos, x, ypos + 10)
        g.drawLine(x, ypos + 10, xpos(tempTime + (baseValue.time.dur * baseValue.time.attack)), ypos)
        g.drawLine(xpos(tempTime + (baseValue.time.dur * baseValue.time.attack)), ypos, xpos(tempTime + baseValue.time.dur), ypos + 10)
        g.drawString(format(tempTime) + " (" + format(baseValue.delta) + ")", x + 10, ypos)

        pulsePart.scaleAndSum(tempTime, baseValue.time.dur).foreach {
          case (start, dur, pattern) => {
            val pulsex = xpos(start)
            g.drawLine(pulsex, ypulsepos, pulsex, ypulsepos + 10)

            pattern.scaleAndSum(start, dur).foreach {
              case (pstart, pdur, amp, ( /*attackCurve, decayCurve*/ attackType, attack)) => {
                val pattx = xpos(pstart)
                g.drawLine(pattx, ypattpos, pattx, ypattpos + 10)
              }
            }
          }

        }
        tempTime = tempTime + baseValue.delta
    }
  }

  def internalPlot1(absoluteTime: Float, track: Int, g: Graphics2D) {
    val ypos = 200 - (track * 30)
    var tempTime = absoluteTime
    bases.foreach {
      baseValue =>
        val x = xpos(tempTime)
        g.drawLine(x, ypos, x, ypos + 10)
        g.drawLine(x, ypos + 10, xpos(tempTime + (baseValue.time.dur * baseValue.time.attack)), ypos)
        g.drawLine(xpos(tempTime + (baseValue.time.dur * baseValue.time.attack)), ypos, xpos(tempTime + baseValue.time.dur), ypos + 10)
        g.drawString(format(tempTime) + " (" + format(baseValue.delta) + ")", x + 10, ypos)
        tempTime = tempTime + baseValue.delta
    }
  }

  def length: Float = bases.foldLeft[Float](0) {
    _ + _.delta
  }

  def tracks: Int = 1
}


case class SequencialPulse(pulses: Seq[Pulse]) extends Pulse {
  def internalPlay(absoluteTime: Float, track: Int)(implicit player: MusicPlayer, layers: Layers) = {
    var temp = absoluteTime
    pulses.foreach {
      pulse =>
        pulse.internalPlay(temp, track)
        temp += pulse.length
    }
  }

  def internalPlot(absolutTime: Float, track: Int, g: Graphics2D) {
    var temp = absolutTime
    pulses.foreach {
      pulse =>
        pulse.internalPlot(temp, track, g)
        temp += pulse.length
    }

  }

  def length: Float = pulses.map(_.length).sum

  def tracks: Int = pulses.map(_.tracks).max
}

case class ParallelPulse(pulses: Seq[Pulse]) extends Pulse {
  def internalPlay(absoluteTime: Float, track: Int)(implicit player: MusicPlayer, layers: Layers) = {
    var tempTrack = track
    pulses.foreach {
      pulse =>
        pulse.internalPlay(absoluteTime, tempTrack)
        tempTrack += 1
    }
  }

  def internalPlot(absolutTime: Float, track: Int, g: Graphics2D) {
    var tempTrack = track
    pulses.foreach {
      pulse =>
        pulse.internalPlot(absolutTime, tempTrack, g)
        tempTrack += 1
    }
  }

  def length: Float = pulses.map(_.length).max

  def tracks: Int = math.max(pulses.length, pulses.map(_.tracks).max)
}


/*
 Patterns
* */

/**
 * Every beat be divided into a scaled pattern. The pattern is scaled to the
 * length of a beat
 * Len: 13, Parts: 1,2,5 => 1.625, 3.25, 8.125
 * @param parts
 */
case class PulseParts(parts: Seq[(Int, PartPattern)]) {

  def scale(totalDuration: Float): Seq[(Float, PartPattern)] = {
    val fact = totalDuration / parts.map(_._1).sum.toFloat
    parts.map {
      case (dur, pattern) =>
        (dur.toFloat * fact, pattern)
    }


  }

  /**
   * Scale the pulses and return pairs of summed time and
   * duration
   *
   * Len: 13, start: 2, Parts: 1,2,5 => (2, 1.625), (3.625, 3.25), (6.875, 8.125)
   * @param start
   * @param totalDuration
   * @return
   */
  def scaleAndSum(start: Float, totalDuration: Float): Seq[(Float, Float, PartPattern)] = {
    var tempTime = start
    scale(totalDuration).map {
      case (dur, pattern) =>
        val pair = (tempTime, dur, pattern)
        tempTime = tempTime + dur
        pair
    }
  }
}

/*
Each part is then further divided into different patterns. Where 0 means time
 1 means duration and 2 means amp.
 pattern11: [[2, 1.5, 0.1], [2, 1.5, 0.2], [3, 0.5, 0.3], [21, 0.1, 0.2], [13, 0.004, 0.4]]

 The last one is AttackCurve
 */


//sealed case class PartPattern(patterns: Seq[(Float, Float, Float, AttackCurve)]) extends Playable {
sealed case class PartPattern(patterns: Seq[(Float, Duration, Float, AttackCurve)]) extends Playable {

  import Pulse._
  import NoteName._
  import GroupName._

  def scale(totalDuration: Float): Seq[(Float, Float, Float, (Either[String, (Float, Float)], Float))] = {
    val fact = totalDuration / patterns.map(_._1).sum.toFloat
    patterns.map {
      case (time, dur, amp, AttackCurve(curveType, attack)) =>
        val tmpTime = fact * time
        //(tmpTime, tmpTime * dur, amp, (curveType, attack.value))
        (tmpTime, dur.getAbsoulteTime(tmpTime), amp, (curveType, attack.value))
    }
  }

  def scaleAndSum(start: Float, totalDuration: Float): Seq[(Float, Float, Float, (Either[String, (Float, Float)], Float))] = {
    var tempTime = start
    scale(totalDuration).map {
      case (time, dur, amp, (curveType, attack)) =>
        val resultPattern = (tempTime, dur, amp, (curveType, attack))
        tempTime = tempTime + time
        resultPattern
    }
  }

  def play()(implicit player: MusicPlayer) = {
    val layers = Layers(1)
    layers.play()
    val track = 0
    val noiseGrain = NoiseGrain2()

    val baseOutput = OutbusArgument(16 + track).arguments
    val patternInput = InbusArgument(16 + track).arguments
    val patternOutput = OutbusArgument(0).arguments

    val baseSourceArgs = BaseArgument(targetNodeId = layers.getGroup(track, SOURCE)).arguments
    val baseGrainArgs = BaseArgument(targetNodeId = layers.getGroup(track, GRAIN)).arguments
    val baseAmp = AmpArgument(0.9f).arguments

    val baseValue = base(HARM_CHORD, (2, 2, SOFT_INVPHI))(0)
    val noteValue = note(noise1)(0)
    val panValue = pan((-1f, -0.6f))(0)
    val tempTime = 0

    player.sendNew(
      noteValue.instrument.arguments ++
        baseSourceArgs ++
        baseValue.time.arguments ++
        baseAmp ++
        panValue.arguments ++
        baseOutput ++
        noteValue.args.flatMap(_.arguments), (tempTime * 1000).round.toLong)


    scaleAndSum(0, 5).foreach {
      case (pstart, pdur, amp, (attackType, attack)) => {
        player.sendNew(noiseGrain.arguments ++
          baseGrainArgs ++
          patternInput ++
          patternOutput ++
          AttackArgument2(attackType).arguments ++
          TimeArgument(pdur, attack).arguments ++
          AmpArgument(amp).arguments,
          (pstart * 1000).round.toLong)
      }
    }
  }
}

import Duration._

object PATTERN1 extends PartPattern(
  Seq((2f, relative(0.1f), 0.1f, SHARP_CURVE),
    (2f, relative(0.5f), 0.2f, SOFT_CURVE),
    (3f, relative(0.5f), 0.3f, SHARP_CURVE),
    (21f, relative(0.1f), 0.9f, SHARP_CURVE),
    (13f, relative(0.004f), 0.4f, SHARP_CURVE)))

// Sharppattern1
// 2 2 2 5 2 2 5
object SHARP_PATTERN1 extends PartPattern(
  Seq((2f, absolute(0.01f), 0.1f, SHARP_CURVE),
    (2f, absolute(0.01f), 0.3f, SHARP_CURVE),
    (2f, absolute(0.01f), 0.2f, SHARP_CURVE),
    (5f, absolute(0.01f), 0.3f, SHARP_CURVE),
    (2f, absolute(0.01f), 0.2f, SHARP_CURVE),
    (2f, absolute(0.01f), 0.3f, SHARP_CURVE),
    (5f, absolute(0.01f), 0.1f, SHARP_CURVE)))

// InvSharppattern1
// 5 2 2 5 2 2 2
object INV_SHARP_PATTERN1 extends PartPattern(
  Seq((5f, absolute(0.01f), 0.1f, SHARP_CURVE),
    (2f, absolute(0.01f), 0.3f, SHARP_CURVE),
    (2f, absolute(0.01f), 0.2f, SHARP_CURVE),
    (5f, absolute(0.01f), 0.3f, SHARP_CURVE),
    (2f, absolute(0.01f), 0.2f, SHARP_CURVE),
    (2f, absolute(0.01f), 0.3f, SHARP_CURVE),
    (2f, absolute(0.01f), 0.1f, SHARP_CURVE)))


// Softpattern1
// 5 3 3 5 3
object SOFT_PATTERN1 extends PartPattern(
  Seq((5f, relative(0.02f), 0.1f, SOFT_CURVE),
    (3f, relative(0.02f), 0.3f, SOFT_CURVE),
    (3f, relative(0.02f), 0.2f, SOFT_CURVE),
    (5f, relative(0.02f), 0.3f, SOFT_CURVE),
    (3f, relative(0.02f), 0.2f, SOFT_CURVE)))

// InvSoftpattern1
// 3 5 3 3 5
object INV_SOFT_PATTERN1 extends PartPattern(
  Seq((3f, relative(0.02f), 0.2f, SOFT_CURVE),
    (5f, relative(0.02f), 0.3f, SOFT_CURVE),
    (3f, relative(0.02f), 0.2f, SOFT_CURVE),
    (3f, relative(0.02f), 0.3f, SOFT_CURVE),
    (5f, relative(0.02f), 0.1f, SOFT_CURVE)

))

//Extra sharp duration 0.001f
// 1 1 2 1 1 1 2 21   1 1 2 1 2 13   2 2 1 1 1 2 2 21  1 2 13
object SHARPER_PATTERN1 extends PartPattern(
  Seq((1, absolute(0.001f), 0.1f, SHARP_CURVE),
      (1, absolute(0.001f), 0.2f, SHARP_CURVE),
      (2, absolute(0.001f), 0.3f, SHARP_CURVE),
      (1, absolute(0.001f), 0.1f, SHARP_CURVE),
      (1, absolute(0.001f), 0.2f, SHARP_CURVE),
      (1, absolute(0.001f), 0.3f, SHARP_CURVE),
      (2, absolute(0.001f), 0.4f, SHARP_CURVE),
      (21, absolute(0.001f), 0.2f, SHARP_CURVE),

    (1, absolute(0.001f), 0.1f, SHARP_CURVE),
    (1, absolute(0.001f), 0.2f, SHARP_CURVE),
    (2, absolute(0.001f), 0.3f, SHARP_CURVE),
    (1, absolute(0.001f), 0.1f, SHARP_CURVE),
    (2, absolute(0.001f), 0.2f, SHARP_CURVE),
    (13, absolute(0.001f), 0.3f, SHARP_CURVE),


    (2, absolute(0.001f), 0.3f, SHARP_CURVE),
    (2, absolute(0.001f), 0.2f, SHARP_CURVE),
    (1, absolute(0.001f), 0.1f, SHARP_CURVE),
    (1, absolute(0.001f), 0.1f, SHARP_CURVE),
    (1, absolute(0.001f), 0.1f, SHARP_CURVE),
    (2, absolute(0.001f), 0.2f, SHARP_CURVE),
    (2, absolute(0.001f), 0.3f, SHARP_CURVE),
    (21, absolute(0.001f), 0.2f, SHARP_CURVE),

    (1, absolute(0.001f), 0.1f, SHARP_CURVE),
    (2, absolute(0.001f), 0.2f, SHARP_CURVE),
    (13, absolute(0.001f), 0.3f, SHARP_CURVE)
))



//Long1
// 5soft 13sharp 2sh 2sh 13sh 5soft 2sh

/*
  val attackTypes: Map[AttackCurveName, AttackCurve] = Map(
    SHARPA -> AttackCurve(10, -10, SHARP_INVPHI), // Sharp attack, Env([0,0.1,0],[0.4, 0.6], [10, -10]).plot
    //               Env([0.01,0.1,0.01],[0.4, 0.6], \exponential).plot
    SOFTA -> AttackCurve(-5, 5, SOFT_INVPHI), // Smooth, Env([0,0.1,0],[0.6, 0.4], [-5, 5]).plot
    //         Env([0.01,0.1,0.01],[0.4, 0.6], \sine).plot
    //         Env([0.01,0.1,0.01],[0.4, 0.6], \welch).plot
    HALFA -> AttackCurve(0, 0, HALF) //Neutral, Env([0,0.1,0],[0.5, 0.5], [0, 0]).plot
    //         Env([0.01,0.1,0.01],[0.4, 0.6], \linear).plot
  )
  val attackTypes2: Map[AttackCurveName, AttackCurve2] = Map(
    SHARPA -> AttackCurve2("exponential", SHARP_INVPHI), // Sharp attack, Env([0,0.1,0],[0.4, 0.6], [10, -10]).plot
    //               Env([0.01,0.1,0.01],[0.4, 0.6], \exponential).plot
    SOFTA -> AttackCurve2("sine", SOFT_INVPHI), // Smooth, Env([0,0.1,0],[0.6, 0.4], [-5, 5]).plot
    //         Env([0.01,0.1,0.01],[0.4, 0.6], \sine).plot
    //         Env([0.01,0.1,0.01],[0.4, 0.6], \welch).plot
    HALFA -> AttackCurve2("linear", HALF) //Neutral, Env([0,0.1,0],[0.5, 0.5], [0, 0]).plot
    //         Env([0.01,0.1,0.01],[0.4, 0.6], \linear).plot
*/

sealed case class AttackCurve(curveType: Either[String, (Float, Float)], attack: AttackType)

object EXTRASHARP_CURVE extends AttackCurve(Right(10f, -10f), SHARP_INVPHI)

object SHARP_CURVE extends AttackCurve(Left("exponential"), SHARP_INVPHI)

object SOFT_CURVE extends AttackCurve(Left("sine"), SOFT_INVPHI)

object HALF_CURVE extends AttackCurve(Left("linear"), HALF)

/*
import music._
import AttackCurve._
import AttackCurveName._
* */

trait Duration {
  def getAbsoulteTime(time: Float): Float
}

object Duration {
  def relative(dur: Float): Duration = RelativeDuration(dur)

  def absolute(dur: Float): Duration = AbsouluteDuration(dur)
}

sealed case class AbsouluteDuration(dur: Float) extends Duration {
  def getAbsoulteTime(time: Float): Float = dur
}

sealed case class RelativeDuration(dur: Float) extends Duration {
  def getAbsoulteTime(time: Float): Float = time * dur
}