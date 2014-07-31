package music

import scala.Array.canBuildFrom

trait InstrumentArgument {
  def arguments: Seq[Object]
}

abstract class RangeArgument[T](start: T, end: T) extends InstrumentArgument {
  def startArgument: String

  def endArgument: String

  def arguments: Seq[Object] = Seq(startArgument, start.asInstanceOf[AnyRef], endArgument, end.asInstanceOf[AnyRef])
}

abstract class RangeFloatArrayArgument(start: Seq[Float], end: Seq[Float]) extends RangeArgument[Seq[Float]](start, end) {
  override def arguments: Seq[Object] = Seq(startArgument, anyToObject(start), endArgument, anyToObject(end))

  def anyToObject(arr: Seq[Float]): Array[Object] =
    arr.toArray.map(_.asInstanceOf[AnyRef])
}

abstract class RangeFloatArgument(start: Float, end: Float) extends RangeArgument[Float](start, end)

case class TimeArgument(dur: Float, attack: Float) extends InstrumentArgument {
  def arguments: Seq[Object] = Seq("dur", dur.asInstanceOf[AnyRef], "attackTime", attack.asInstanceOf[AnyRef])
}

case class FrequenciesArgument(start: Seq[Float], end: Seq[Float]) extends RangeFloatArrayArgument(start, end) {
  def startArgument: String = "startFreqs"

  def endArgument: String = "endFreqs"
}

case class BandwithsArgument(start: Seq[Float], end: Seq[Float]) extends RangeFloatArrayArgument(start, end) {
  def startArgument: String = "startBws"

  def endArgument: String = "endBws"
}

case class AttackArgument(start: Float = 0, end: Float = 0) extends RangeFloatArgument(start, end) {
  def startArgument: String = "attackCurve"

  def endArgument: String = "decayCurve"
}

case class AttackArgument2(attackType: Either[String, (Float, Float)]) extends InstrumentArgument {
  def arguments: Seq[Object] = {
    attackType match {
      case Left(attackType) => Array("attackType", attackType)
      case Right((start, end)) => Array("attackType", Array(start, end))
    }

  }

}

case class PanArgument(start: Float, end: Float) extends RangeFloatArgument(start, end) {
  def startArgument: String = "startPan"

  def endArgument: String = "endPan"
}

case class NodeArgument(addAction: Integer = 0, targetNodeId: Integer = 0) extends InstrumentArgument {
  def arguments: Seq[Object] = Array(addAction, targetNodeId)
}

case class InbusArgument(inbus: Integer = 0) extends InstrumentArgument {
  def arguments: Seq[Object] = Array("inbus", inbus)
}

case class OutbusArgument(outbus: Integer = 0) extends InstrumentArgument {
  def arguments: Seq[Object] = Array("outbus", outbus)
}

case class AmpArgument(amp: Float) extends InstrumentArgument {
  def arguments: Seq[Object] = Array("amp", amp.asInstanceOf[AnyRef])
}

case class BaseArgument(synthId: Integer = -1, addAction: Integer = 0, targetNodeId: Integer = 0) extends InstrumentArgument {
  def arguments: Seq[Object] = Array(synthId, addAction, targetNodeId)

}

abstract class InstrumentName extends InstrumentArgument {
  def instrumentName: String

  def arguments: Seq[Object] = Array(instrumentName)

}

case class InvertedSpektrum4() extends InstrumentName {
  def instrumentName: String = "invertedSpektrum42"
}

case class InvertedSpektrum6() extends InstrumentName {
  def instrumentName: String = "invertedSpektrum63"
}

case class Spektrum() extends InstrumentName {
  def instrumentName: String = "spektrum42"
}

case class NoiseGrain() extends InstrumentName {
  def instrumentName: String = "noiseGrain"
}

case class NoiseGrain2() extends InstrumentName {
  def instrumentName: String = "noiseGrain2"
}