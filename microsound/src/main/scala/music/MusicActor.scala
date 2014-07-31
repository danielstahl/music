package music

import music.MusicActor._


object MusicActor {

  /**
   * [PIA <: PatternItem[MusicActor], MusicActorPattern <% Pattern[MusicActor, PIA]]
   */
  type MusicActorPattern = Pattern[MusicActor, PatternItem[MusicActor]]
  type TimeItemBuilderPattern = Pattern[TimeItemBuilder, PatternItem[TimeItemBuilder]]
}

/**
 * Base trait for all actors.
 */
trait MusicActor {
  def receive: PartialFunction[MusicEvent, Unit]

  private def aroundReceive(event: MusicEvent) = receive.applyOrElse(event, unhandled)

  private def unhandled(event: MusicEvent): Unit = {
    event match {
      case _  => sys.error(s"$event is unhandled by ${this.getClass.getSimpleName}")
    }
  }

  def tell(event: MusicEvent) {
    //println(s"${this.getClass.getSimpleName} got message $event")
    aroundReceive(event)
  }
}

trait MusicEvent

case class DurationEvent(duration: Float) extends MusicEvent

case class PrinterActor() extends MusicActor {
  def receive = {
      case event: MusicEvent => println(event)
  }
}

case class TimeItemEvent(timeItem: TimeItem) extends MusicEvent

case class TimeItemBuilderActor(timeItemBuilders: TimeItemBuilderPattern, listeners: MusicActorPattern) extends MusicActor {

  def receive = {
    case TimeItemEvent(timeItem) =>
      val timeItems = timeItemBuilders.takeItem().build(timeItem.start, timeItem.duration)
      listeners.takeItem().tell(TimeItemsEvent(timeItems))
  }
}

case class TimeItemsEvent(timeItems: List[TimeItem]) extends MusicEvent

case class TimeItemSplitterActor(listeners: MusicActorPattern) extends MusicActor {
  def receive = {
    case TimeItemsEvent(timeItems) =>
      timeItems.foreach {
        timeItem =>
        listeners.takeItem().tell(TimeItemEvent(timeItem))
    }
  }
}

case class TimeItemsTransformerActor(transformer: TimeItemTransformer, listeners: MusicActorPattern) extends MusicActor {
  override def receive: PartialFunction[MusicEvent, Unit] = {
    case TimeItemsEvent(timeItems) =>
      listeners.takeItem().tell(TimeItemsEvent(transformer.transform(timeItems)))
  }
}




