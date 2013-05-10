package music

import music._
import SpectrumName._
import Harmony._
import com.illposed.osc.OSCPacket
import com.illposed.osc.OSCMessage
import com.illposed.osc.OSCPortOut
import java.util.Date
import com.illposed.osc.OSCBundle
import java.net.InetAddress

object Bandwith {
  private val bws: Seq[Float] = Seq(0.01f, 0.001f, 0.0001f, 0.00001f, 0.00000001f, 0.000000001f)

  def apply(indeces: Int*): Seq[Float] = {
    indeces.map(bws(_))
  }
}

object NoteName extends Enumeration {
  type NoteName = Value
  val noise1, noise2, noise3, noise4, noise5, harm1, harm2, harm3, harm4, harm5 = Value
}

case class LongNote(instrument: InstrumentName, args: Seq[InstrumentArgument]) extends Playable {
  private val defaultBase: BaseArgument = BaseArgument()
  private val defaultTime: TimeArgument = TimeArgument(13f, 0.5f)

  def play()(implicit player: MusicPlayer) = {
    player.sendNew(instrument.arguments ++ defaultBase.arguments ++ defaultTime.arguments ++ args.flatMap(_.arguments), 0)
  }
}

/*
 Top - Pulse with beat. 
 Several pulses sometimes together but sometimes drift apart
 Pulse 1 13 13 13 8 5 3  8 8 8
 Pulse 2 13 13 8  13 3 5 8 8 8
 
 The pulse is used to play the long notes.
 
 Every beat be divided into a scaled pattern. The pattern in scaled to the 
 length of a beat
 Len: 13, Parts: 1,2,5 => 1.625, 3.25, 8.125 
 
 Each part is then further divided into different patterns. Where 0 means time
 1 means duration and 2 means amp.
 pattern11: [[2, 1.5, 0.1], [2, 1.5, 0.2], [3, 0.5, 0.3], [21, 0.1, 0.2], [13, 0.004, 0.4]]
 
 This pattern mean that the part is divided into the times 2,2,3,21,13. The durations are
 2*1.5, 2*1.5, 3*0.5, 21*0.1 and 13* 0.004
 
 * */



case class Parts(time: Float, parts: Seq[Int]) {
  def makeParts(): Seq[Float] = {
    val theSum: Float = parts.reduceLeft(_ + _)
    parts.map {part => time * (part / theSum)}
  }
}


object Music {
  import NoteName._

  val player: MusicPlayer = MusicPlayer()
  val plotter: DataPlotter = DataPlotter()
  
  val longNotes: Map[NoteName, LongNote] = Map(
    noise1 -> LongNote(InvertedSpektrum4(), Seq(
      FrequenciesArgument(
        harmony(HARMON).octave(2).chord(1, 7, 15, 18),
        harmony(HARMON).octave(2).chord(1, 6, 12, 19)),
      BandwithsArgument(Bandwith(2, 2, 2, 2), Bandwith(0, 0, 0, 0)),
      AttackArgument(0, 0))),
      
    noise2 -> LongNote(InvertedSpektrum6(), Seq(
      FrequenciesArgument(
        harmony(HARMON).octave(5).chord(0, 1, 2, 3, 4, 5),
        harmony(HARMON).octave(5).chord(0, 1, 2, 3, 4, 5)),
      BandwithsArgument(Bandwith(2, 2, 2, 2, 2, 2), Bandwith(2, 2, 2, 2, 2, 2)),
      AttackArgument(0, 0))),
      
    // Isnt working  
    noise3 -> LongNote(InvertedSpektrum6(), Seq(
      FrequenciesArgument(
        harmony(HARMON).octave(7).chord(0, 1, 2, 3, 4, 5),
        harmony(HARMON).octave(7).chord(0, 1, 2, 3, 4, 5)),
      BandwithsArgument(Bandwith(2, 2, 2, 2, 2, 2), Bandwith(2, 2, 2, 2, 2, 2)),
      AttackArgument(0, 0))),
    
    noise4 -> LongNote(InvertedSpektrum6(), Seq(
      FrequenciesArgument(
        harmony(HARMON).octave(5).chord(0,6,8,10,12,19),
        harmony(HARMON).octave(5).chord(0,6,8,10,12,19)
      ),
      BandwithsArgument(Bandwith(5,5,5,5,5,5), Bandwith(5,5,5,5,5,5)),
      AttackArgument(0,0))),
   noise5 -> LongNote(InvertedSpektrum6(), Seq(
     FrequenciesArgument(
       harmony(HARMON).octave(6).chord(0,6,8,10,12,19),
       harmony(HARMON).octave(6).chord(0,6,8,10,12,19)),
     BandwithsArgument(Bandwith(2,2,2,2,2,2), Bandwith(2,2,2,2,2,2)),
     AttackArgument(0,0))),
    
    harm1 -> LongNote(Spektrum(), Seq(
      FrequenciesArgument(
        harmony(HARMON).octave(6).chord(1,7,15,18),
        harmony(HARMON).octave(6).chord(1,6,17,19)),
      BandwithsArgument(Bandwith(1,1,1,1), Bandwith(3,3,3,3)),
      AttackArgument(0,0))),
      
    harm2 -> LongNote(Spektrum(), Seq(
      FrequenciesArgument(
        harmony(HARMON).octave(4).chord(1,5,10,12),
        harmony(HARMON).octave(4).chord(1,3,11,18)),
      BandwithsArgument(Bandwith(1,1,1,1), Bandwith(3,3,3,3)),
      AttackArgument(0,0))),
      
    harm3 -> LongNote(Spektrum(), Seq(
      FrequenciesArgument(
        harmony(HARMON).octave(8).chord(1,2,3,4),
        harmony(HARMON).octave(8).chord(0,3,5,7)),
      BandwithsArgument(Bandwith(2,2,2,2), Bandwith(3,3,3,3)),
      AttackArgument(0,0))),
      
    harm4 -> LongNote(Spektrum(), Seq(
      FrequenciesArgument(
        harmony(HARMON).octave(9).chord(0,3,5,7),
        harmony(HARMON).octave(9).chord(1,2,3,4)),
      BandwithsArgument(Bandwith(2,2,2,2), Bandwith(1,1,1,1)),
      AttackArgument(0,0))),
      
    harm5 -> LongNote(Spektrum(), Seq(
      FrequenciesArgument(
        harmony(HARMON).octave(3).chord(0,7,15,19),
        harmony(HARMON).octave(3).chord(1,6,8,9)),
      BandwithsArgument(Bandwith(1,1,1,1), Bandwith(3,3,3,3)),   
      AttackArgument(0,0)))
  )
}