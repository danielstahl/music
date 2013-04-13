package music

import Common._
import org.scalatest.FunSuite

class CommonTest extends FunSuite {
	test("make serie") {	  
	  assert(makeSerie(3, 1.5f, 3) === Seq[Float](3.0f, 4.5f, 6.75f))
	}
	
	test("make spektrum") {
	  assert(makeSpectrum(110, 2, 5) === Seq[Float](110.0f, 330.0f, 550.0f, 770.0f, 990.0f))
	}
	
	test("make step serie") {
	  assert(makeStepSerie(2, 3) === Seq[Int](2, 3, 4))
	}
	
	test("make inverted step serie") {                          
	  assert(makeInvertedStepSerie(20, 4) === Seq[Float](0.05f, 0.047619047619047616f, 0.045454545454545456f, 0.043478260869565216f))
	}
	
}