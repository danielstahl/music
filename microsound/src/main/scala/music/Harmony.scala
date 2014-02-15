package music

import Common.phi
import Common.invPhi
import java.awt.Graphics2D

case class Octave(spectrums: Seq[Spectrum]) {
  def octave(i: Int): Spectrum = {
    spectrums(i)
  }
}


object SpectrumName extends Enumeration {
  type SpectrumName = Value
  val PHI, INVERTED_PHI, HARMON = Value
}


case class Spectrum(spekt: Seq[Float]) extends Plottable {
  def chord(indices: Int*): Seq[Float] = {
    indices.map(spekt(_))
  }
  
  private val topMargin = 20
  private val scaleTo = 600

  def plot(g: Graphics2D) = {
    plot(g, 10, spekt.max)
  }
  
  def plot(g: Graphics2D, xpos: Int, max: Float) = {
	val theScale = scaleTo / max
    spekt.map(
      specValue => {
        val theValue = (theScale * specValue).intValue
        val mirrorValue = (theValue * -1) + scaleTo + topMargin
        g.drawLine(xpos, mirrorValue, xpos + 10, mirrorValue)
        g.drawString(specValue.toString, xpos + 15, mirrorValue)
      })
  }
}

case class Spectrums(spectrums: Seq[Spectrum]) extends Plottable {
	def plot(g: Graphics2D) = {
	  val max: Float = spectrums.map(_.spekt.max).max
	  spectrums.indices.foreach(i => spectrums(i).plot(g, 10 + (i * 100), max))
	}
}

object Harmony {
  import SpectrumName._

  private val BASE_FREQ = 1.71875f
  private val OCTAVE_SIZE = 10
  private val SPECT_SIZE = 20

  private def makeSerie(fakt: Float): Seq[Float] =
    Common.makeSerie(BASE_FREQ, fakt, OCTAVE_SIZE)

  private def makeSpectrum(base: Float, fakt: Float): Spectrum =
    Spectrum(Common.makeSpectrum(base, fakt, SPECT_SIZE))


  def harm(serie: SpectrumName.Value, octave: Int, value: Int): Float =
    harmony(serie).octave(octave).spekt(value)
  
  def harm(serie: SpectrumName.Value, octave: Int, value: Int*): Seq[Float] =
    harmony(serie).octave(octave).chord(value:_*)
  
  def harm(serie: SpectrumName.Value, octave: Int): Seq[Float] =
    harmony(serie).octave(octave).spekt
  
  def spect(serie: SpectrumName.Value, octave: Int): Spectrum =
    harmony(serie).octave(octave)
   
  private val harmony: Map[SpectrumName, Octave] =
    Map(
      HARMON -> Octave(makeSerie(2).map(oct => makeSpectrum(oct, 1))),
      PHI -> Octave(makeSerie(2 * phi).map(oct => makeSpectrum(oct, phi))),
      INVERTED_PHI -> Octave(makeSerie(2 * invPhi).map(oct => makeSpectrum(oct, invPhi))))
      
  def apply(spectrumName: SpectrumName): Octave = harmony(spectrumName)
}
