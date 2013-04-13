package music

import com.illposed.osc._
import java.util.Date
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
	
    def anyToObject(arr: Seq[Float]): Array[Object] = {
    	arr.toArray.map(_.asInstanceOf[AnyRef])
  }
}

abstract class RangeFloatArgument(start: Float, end: Float) extends RangeArgument[Float](start, end) {
  
}

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

case class AttackArgument(time: Float, start: Float = 0, end: Float = 0) extends RangeFloatArgument(start, end) {
	def startArgument: String = "attackCurve"
	def endArgument: String = "decayCurve"	 
}

case class PanArgument(start: Float, end: Float) extends RangeFloatArgument(start, end) {
	def startArgument: String = "startPan"
	def endArgument: String = "endPan"
}

case class NodeArgument(addAction: Integer = 0, targetNodeId: Integer = 0) extends InstrumentArgument {
  def arguments: Seq[Object] = {
    Array(addAction, targetNodeId)
  }
}

case class BusArgument(in: Integer = 1000, out: Integer = 1000) extends InstrumentArgument {
  def arguments: Seq[Object] = {
    Array(in, out)
  }
}

case class BaseArgument(bus: Integer = -1, addAction: Integer = 0, targetNodeId: Integer = 0) extends InstrumentArgument {  
  def arguments: Seq[Object] = {
    Array(bus, addAction, targetNodeId)
  }
}

abstract class InstrumentName extends InstrumentArgument {
	def instrumentName: String
  
	def arguments: Seq[Object] = {
	    Array(instrumentName)
	}
}

case class InvertedSpektrum4 extends InstrumentName {
  def instrumentName: String = "invertedSpektrum42"  
}

case class InvertedSpektrum6 extends InstrumentName {
  def instrumentName: String = "invertedSpektrum63"  
}

case class Spektrum extends InstrumentName {
  def instrumentName: String = "spektrum42"  
}	
