package music


object NoteName extends Enumeration {
  type NoteName = Value
  val noise1, noise2, noise3, noise4, noise5, harm1, harm2, harm3, harm4, harm5 = Value
}

object Bandwith {
  private val bws: Seq[Float] = Seq(0.01f, 0.001f, 0.0001f, 0.00001f, 0.00000001f, 0.000000001f)

  def apply(indeces: Int*): Seq[Float] = {
    indeces.map(bws(_))
  }
}

object LongNote {
  import NoteName._
  import SpectrumName._
  
  def apply(noteName: NoteName): LongNote = longNotes(noteName)
  
	private val longNotes: Map[NoteName, LongNote] = Map(
    noise1 -> LongNote(InvertedSpektrum4(), Seq(
      FrequenciesArgument(
        Harmony(HARMON).octave(2).chord(1, 7, 15, 18),
        Harmony(HARMON).octave(2).chord(1, 6, 12, 19)),
      BandwithsArgument(Bandwith(2, 2, 2, 2), Bandwith(0, 0, 0, 0)),
      AttackArgument(0, 0))),

    noise2 -> LongNote(InvertedSpektrum6(), Seq(
      FrequenciesArgument(
        Harmony(HARMON).octave(5).chord(0, 1, 2, 3, 4, 5),
        Harmony(HARMON).octave(5).chord(0, 1, 2, 3, 4, 5)),
      BandwithsArgument(Bandwith(2, 2, 2, 2, 2, 2), Bandwith(2, 2, 2, 2, 2, 2)),
      AttackArgument(0, 0))),

    // Isnt working  
    noise3 -> LongNote(InvertedSpektrum6(), Seq(
      FrequenciesArgument(
        Harmony(HARMON).octave(7).chord(0, 1, 2, 3, 4, 5),
        Harmony(HARMON).octave(7).chord(0, 1, 2, 3, 4, 5)),
      BandwithsArgument(Bandwith(2, 2, 2, 2, 2, 2), Bandwith(2, 2, 2, 2, 2, 2)),
      AttackArgument(0, 0))),

    noise4 -> LongNote(InvertedSpektrum6(), Seq(
      FrequenciesArgument(
        Harmony(HARMON).octave(5).chord(0, 6, 8, 10, 12, 19),
        Harmony(HARMON).octave(5).chord(0, 6, 8, 10, 12, 19)),
      BandwithsArgument(Bandwith(5, 5, 5, 5, 5, 5), Bandwith(5, 5, 5, 5, 5, 5)),
      AttackArgument(0, 0))),
    noise5 -> LongNote(InvertedSpektrum6(), Seq(
      FrequenciesArgument(
        Harmony(HARMON).octave(6).chord(0, 6, 8, 10, 12, 19),
        Harmony(HARMON).octave(6).chord(0, 6, 8, 10, 12, 19)),
      BandwithsArgument(Bandwith(2, 2, 2, 2, 2, 2), Bandwith(2, 2, 2, 2, 2, 2)),
      AttackArgument(0, 0))),

    harm1 -> LongNote(Spektrum(), Seq(
      FrequenciesArgument(
        Harmony(HARMON).octave(6).chord(1, 7, 15, 18),
        Harmony(HARMON).octave(6).chord(1, 6, 17, 19)),
      BandwithsArgument(Bandwith(1, 1, 1, 1), Bandwith(3, 3, 3, 3)),
      AttackArgument(0, 0))),

    harm2 -> LongNote(Spektrum(), Seq(
      FrequenciesArgument(
        Harmony(HARMON).octave(4).chord(1, 5, 10, 12),
        Harmony(HARMON).octave(4).chord(1, 3, 11, 18)),
      BandwithsArgument(Bandwith(1, 1, 1, 1), Bandwith(3, 3, 3, 3)),
      AttackArgument(0, 0))),

    harm3 -> LongNote(Spektrum(), Seq(
      FrequenciesArgument(
        Harmony(HARMON).octave(8).chord(1, 2, 3, 4),
        Harmony(HARMON).octave(8).chord(0, 3, 5, 7)),
      BandwithsArgument(Bandwith(2, 2, 2, 2), Bandwith(3, 3, 3, 3)),
      AttackArgument(0, 0))),

    harm4 -> LongNote(Spektrum(), Seq(
      FrequenciesArgument(
        Harmony(HARMON).octave(9).chord(0, 3, 5, 7),
        Harmony(HARMON).octave(9).chord(1, 2, 3, 4)),
      BandwithsArgument(Bandwith(2, 2, 2, 2), Bandwith(1, 1, 1, 1)),
      AttackArgument(0, 0))),

    harm5 -> LongNote(Spektrum(), Seq(
      FrequenciesArgument(
        Harmony(HARMON).octave(3).chord(0, 7, 15, 19),
        Harmony(HARMON).octave(3).chord(1, 6, 8, 9)),
      BandwithsArgument(Bandwith(1, 1, 1, 1), Bandwith(3, 3, 3, 3)),
      AttackArgument(0, 0))))
}

case class LongNote(instrument: InstrumentName, args: Seq[InstrumentArgument]) extends Playable {
  private val defaultBase: BaseArgument = BaseArgument()
  private val defaultTime: TimeArgument = TimeArgument(13f, 0.5f)

  def play()(implicit player: MusicPlayer) = {
    player.sendNew(instrument.arguments ++ defaultBase.arguments ++ defaultTime.arguments ++ args.flatMap(_.arguments), 0)
  }
}
