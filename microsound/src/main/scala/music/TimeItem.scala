package music

case class TimeItem(start: Float, duration: Float)

object TimeItem {
  def timeAtom = TimeAtomBuilder

  def patternScaledTime(pattern: Pattern[TimeItemBuilder, PatternItem[TimeItemBuilder]]) =
    PatternTimeItemBuilder(pattern)

  def pulseScaledTime(steps: Int, scaledTime: TimeItemBuilder = TimeAtomBuilder) =
    PulseTimeBuilder(steps, scaledTime)

  def relativeScaledTime(parts: (Int, TimeItemBuilder)*) =
    RelativeTimeBuilder(parts.toList)

  def pulsePhase(scaledTime: TimeItemBuilder, repeats: Int = 1, phases: Int = 1, phase: Float = 1) =
    PulsePhase(scaledTime, repeats, phases, phase)
}

trait TimeItemBuilder {
  def build(startTime: Float, totalDuration: Float): List[TimeItem] = {
    buildAbsoluteTime(startTime, buildRelativeTime(totalDuration))
  }

  def buildRelativeTime(totalDuration: Float): List[Float]

  protected def buildAbsoluteTime(startTime: Float, parts: List[Float]): List[TimeItem] = {
    var tempTime = startTime
    parts.map {
      duration =>
        val item = TimeItem(tempTime, duration)
        tempTime = tempTime + duration
        item
    }
  }
}

object TimeAtomBuilder extends TimeItemBuilder {
  override def buildRelativeTime(totalDuration: Float): List[Float] = List(totalDuration)
}

case class PatternTimeItemBuilder(pattern: Pattern[TimeItemBuilder, PatternItem[TimeItemBuilder]]) extends TimeItemBuilder {
  override def buildRelativeTime(totalDuration: Float): List[Float] = pattern.takeItem().buildRelativeTime(totalDuration)
}

case class PulseTimeBuilder(steps: Int, scaledTime: TimeItemBuilder) extends TimeItemBuilder {
  def buildRelativeTime(totalDuration: Float): List[Float] = {
    val fact = totalDuration / steps.toFloat
    (1 to steps).map(i => scaledTime.buildRelativeTime(fact)).flatten.toList
  }
}

case class RelativeTimeBuilder(parts: List[(Int, TimeItemBuilder)]) extends TimeItemBuilder {
  def buildRelativeTime(totalDuration: Float): List[Float] = {
    val fact = totalDuration / parts.map(_._1).sum.toFloat
    parts.map {
      case (part, scaledTime) => scaledTime.buildRelativeTime(part * fact)
    }.flatten.toList
  }
}



trait TimeItemTransformer {
  def transform(items: List[TimeItem]): List[TimeItem]
}

case class PulseTransformer(repeats: Int = 1) extends TimeItemTransformer {
  def transform(items: List[TimeItem]): List[TimeItem] = {
    val duration = items.map(_.duration).sum
    items ++ (1 to repeats).flatMap {
      i =>
       val startTime = i * duration
       items.map {
         case TimeItem(start, dur) => TimeItem(startTime + start, dur)
       }
    }
  }
}

case class ScaleTransformer(factor: Float = 1) extends TimeItemTransformer {
  def transform(items: List[TimeItem]): List[TimeItem] = {
    val startTime = items.head.start

    val newItems = items.foldLeft((startTime, List[TimeItem]())) {
      case ((nextStart, result), timeItem) => {
        val newDuration = timeItem.duration * factor
        (nextStart + newDuration, result ::: List(TimeItem(nextStart, newDuration)))
      }
    }
    newItems._2
  }
}

case class ChainedTimeItemTransformer(transformers: TimeItemTransformer*) extends TimeItemTransformer {
  override def transform(items: List[TimeItem]): List[TimeItem] = {
    transformers.foldLeft(items) {
      case (theItems, transformer) => transformer.transform(theItems)
    }
  }
}

case class PatternTimeItemTransformer(pattern: Pattern[TimeItemTransformer, PatternItem[TimeItemTransformer]]) extends TimeItemTransformer {
  override def transform(items: List[TimeItem]): List[TimeItem] = {
    pattern.takeItem().transform(items)
  }
}

@deprecated
case class PulsePhase(scaledTime: TimeItemBuilder, repeats: Int = 1, phases: Int = 1, phase: Float = 1) {
  def repeat(startTime: Float, totalDuration: Float): List[List[TimeItem]] = {
    var currentDuration = totalDuration
    (1 to phases).map {_ =>
        var currentStartTime = startTime
        val tempPhase = (1 to repeats).flatMap {_ =>
            val tempRepeat = scaledTime.build(currentStartTime, currentDuration)
            currentStartTime += currentDuration
            tempRepeat
        }
        currentDuration = currentDuration + (currentDuration * phase)
        tempPhase.toList
    }.toList


  }.toList
}
