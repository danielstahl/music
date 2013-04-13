package music

import Harmony._

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

  val pulseChords: Map[PulseChordName.Value, PulseChord] =
    Map(
      HARM -> PulseChord(harm(SpectrumName.HARMON, 0, 3, 4, 5, 6, 7)),
      PHI -> PulseChord(harm(SpectrumName.PHI, 0, 1, 2, 3, 4, 5)),
      IPHI -> PulseChord(harm(SpectrumName.INVERTED_PHI, 0, 5, 6, 7, 8, 9)))

  def simplePulse(name: PulseChordName.Value, indices: Int*): SimplePulse = {
    SimplePulse(pulseChords(name).vals(indices: _*))
  }

  def parallelPulse(pulses: Pulse*): ParallelPulse = {
    ParallelPulse(pulses)
  }

  def sequencialPulse(pulses: Pulse*): SequencialPulse = {
    SequencialPulse(pulses)
  }

  val pulse: Pulse =
    sequencialPulse(
      simplePulse(HARM, 2, 2, 2),
      parallelPulse(
        simplePulse(HARM, 1, 0, 0, 1, 0),
        simplePulse(HARM, 0, 0, 1, 0, 1)),
      simplePulse(HARM, 1, 1, 1))
}

trait Pulse {

}

case class SimplePulse(vals: Seq[Float]) extends Pulse {
}

case class SequencialPulse(pulses: Seq[Pulse]) extends Pulse {

}

case class ParallelPulse(pulses: Seq[Pulse]) extends Pulse {

}

case class PulseChord(values: Seq[Float]) {
  def vals(indices: Int*): Seq[Float] = {
    indices.map(values(_))
  }
}