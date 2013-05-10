package music

import Harmony._
import java.awt.Graphics2D

/**
 * harm(HARMON, 0, 3, 4, 5, 6, 7)
 * 6.875, 8.59375, 10.3125, 12.03125, 13.75
 * harm(PHI, 0, 1, 2, 3, 4, 5)
 * 4.499746, 7.2807417, 10.061738, 12.842733, 15.62373
 * harm(INVERTED_PHI, 0, 5, 6, 7, 8, 9)
 * 7.0299797, 8.092226, 9.154471, 10.216718, 11.278963
 */

object PulseChordName extends Enumeration {
  val PHI, IPHI, HARM = Value
}

object Time {
  import PulseChordName._
  import NoteName._
  import Common._

  val pulseChords: Map[PulseChordName.Value, PulseChord] =
    Map(
      HARM -> PulseChord(harm(SpectrumName.HARMON, 0, 3, 4, 5, 6, 7)),
      PHI -> PulseChord(harm(SpectrumName.PHI, 0, 1, 2, 3, 4, 5)),
      IPHI -> PulseChord(harm(SpectrumName.INVERTED_PHI, 0, 5, 6, 7, 8, 9)))

  def simplePulse(bases: Seq[PulseTime], notes: Seq[LongNote], pans: Seq[PanArgument]): SimplePulse = {
    SimplePulse(bases, notes, pans)
  }

  def base(name: PulseChordName.Value, bases: (Int, Int, Float)*): Seq[PulseTime] = {
    val chord = pulseChords(name)
    bases.map {
      base => PulseTime(chord.values(base._1), TimeArgument(chord.values(base._2), base._3))
    }
  }

  def note(names: NoteName.Value*): Seq[LongNote] = {
    names.map {
      noteName => Music.longNotes(noteName)
    }
  }

  def pan(pans: (Int, Int)*): Seq[PanArgument] = {
    pans.map {
      pan => PanArgument(pan._1, pan._2)
    }
  }

  def parallelPulse(pulses: Pulse*): ParallelPulse = {
    ParallelPulse(pulses)
  }

  def sequencialPulse(pulses: Pulse*): SequencialPulse = {
    SequencialPulse(pulses)
  }

  val pulse: Pulse =
    sequencialPulse(
      simplePulse(base(HARM, (2, 2, invPhi), (2, 2, invPhi), (2, 2, invPhi)),
        note(noise1, noise2, noise1),
        pan((-1, 1), (-1, 1), (-1, 1))),
      parallelPulse(
        simplePulse(base(HARM, (1, 1, invPhi), (0, 0, invPhi), (1, 1, invPhi), (0, 0, invPhi), (0, 0, invPhi)),
          note(noise1, noise1, noise1, noise1, noise1),
          pan((-1, 0), (-1, 0), (-1, 0), (-1, 0), (-1, 0))),
        simplePulse(base(HARM, (0, 0, invPhi), (0, 0, invPhi), (1, 1, invPhi), (0, 0, invPhi), (1, 1, invPhi)),
          note(noise2, noise2, noise2, noise2, noise2),
          pan((-1, 0), (-1, 0), (-1, 0), (-1, 0), (-1, 0)))),

      simplePulse(base(HARM, (1, 1, invPhi), (1, 1, invPhi), (1, 1, invPhi)),
        note(noise2, noise1, noise2),
        pan((-1, 0), (-1, 0), (-1, 0))))

}

case class PulseTime(delta: Float, time: TimeArgument) {

}

trait Pulse extends Plottable with Playable {
  
  def play()(implicit player: MusicPlayer) = {
	  internalPlay(0)
  }
  
  def internalPlay(absoluteTime: Float)(implicit player: MusicPlayer) 
  
  def plot(g: Graphics2D) {
    internalPlot(0, 0, g)
  }

  def internalPlot(absolutTime: Float, track: Int, g: Graphics2D)

  def length: Float

  def format(theValue: Number): String = {
    "%4.2f" format theValue
  }
}

case class SimplePulse(bases: Seq[PulseTime], notes: Seq[LongNote], pans: Seq[PanArgument]) extends Pulse {
	private val defaultBase: BaseArgument = BaseArgument()
	
  def internalPlay(absoluteTime: Float)(implicit player: MusicPlayer) = {
	  
	  var tempTime = absoluteTime
	  (bases, notes, pans).zipped.foreach {
	    (baseValue, noteValue, panValue) =>
	      println("note at " + (tempTime * 1000).round.toLong + " with duration " + baseValue.time.dur)
	      player.sendNew(noteValue.instrument.arguments ++ 
	          defaultBase.arguments ++ 
	          baseValue.time.arguments ++
	          panValue.arguments ++
	          noteValue.args.flatMap(_.arguments), (tempTime * 1000).round.toLong)
	      
	      tempTime = tempTime + baseValue.delta
	  }
  }
  
  def xpos(xtime: Float): Int = (10 + (xtime * 10).round).toInt

  def internalPlot(absoluteTime: Float, track: Int, g: Graphics2D) {
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

  def length: Float = bases.foldLeft[Float](0) { _ + _.delta }
}

case class SequencialPulse(pulses: Seq[Pulse]) extends Pulse {
 
  def internalPlay(absoluteTime: Float)(implicit player: MusicPlayer) = {
    var temp = absoluteTime
    pulses.foreach {
      pulse =>
        pulse.internalPlay(temp)
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
}

case class ParallelPulse(pulses: Seq[Pulse]) extends Pulse {
  
  def internalPlay(absoluteTime: Float)(implicit player: MusicPlayer) = {
	pulses.foreach {
      pulse =>
        pulse.internalPlay(absoluteTime)
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
}

case class PulseChord(values: Seq[Float]) {
  def vals(indices: Int*): Seq[Float] = {
    indices.map(values(_))
  }
}

