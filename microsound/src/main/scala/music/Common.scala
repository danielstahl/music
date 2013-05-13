package music

object Common {
  val phi: Float = (1 + Math.sqrt(5).asInstanceOf[Float]) / 2
  
  val invPhi: Float = 1 / phi
  
  def makeSerie(start: Float, fakt: Float, size: Int): Seq[Float] = {
	  var temp:Float  = start
	  (0 until size).map(i => {
	    if(i == 0) {
	    	start
	    } else {
	    	temp = temp * fakt
	    	temp
	    }		  
	  })		  
  }

   def makeSpectrum(base: Float, fact: Float, size: Int): Seq[Float] = {
    for {
    	i <- 0 until size
    	mult: Float = fact * ((i + 1) - 1) + 1
      } yield base * mult
   }
  
   def makeStepSerie(start: Int, size: Int): Seq[Int] = {
     for {
       i <- 0 until size
     } yield i + start
   }
   
   def makeInvertedStepSerie(start: Int, size: Int): Seq[Float] = {
     for {
       i <- 0 until size
     } yield 1f / (i + start)
   }
}