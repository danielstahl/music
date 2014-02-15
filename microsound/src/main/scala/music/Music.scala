package music

import music._
import Harmony._
import com.illposed.osc.OSCPacket
import com.illposed.osc.OSCMessage
import com.illposed.osc.OSCPortOut
import java.util.Date
import com.illposed.osc.OSCBundle
import java.net.InetAddress
import music.GroupName.GroupName

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
 
 */

/*
OSC reference
http://doc.sccode.org/Reference/Server-Command-Reference.html

Busses
http://doc.sccode.org/Tutorials/Getting-Started/11-Busses.html

Use the ServerOptions to find out wich buses are private (not connected to hardware) and wich
who are "public"
http://doc.sccode.org/Classes/ServerOptions.html
s.options.firstPrivateBus will tell you that


Groups
the base group is 0

* */

object GroupName extends Enumeration {
  type GroupName = Value
  val SOURCE, GRAIN, EFFECT = Value
}


case class Layers(size: Int) extends Playable {
  import GroupName._

  var layers: Seq[Map[GroupName, Int]] =
    (0 until size).map {
      l => Map(SOURCE -> node(l, SOURCE), GRAIN -> node(l, GRAIN), EFFECT -> node(l, EFFECT))
    }

  private def node(layer: Int, name: GroupName) = name match {
    case SOURCE => 1000 + (layer * 10)
    case GRAIN => 1000 + (layer * 10) + 1
    case EFFECT => 1000 + (layer * 10) + 2

  }

  def play()(implicit player: MusicPlayer) = {

    val osc = (0 until size).flatMap {
      l =>
        Seq(player.makeGroupHead(0, node(l, SOURCE)),
          player.makeGroupTail(node(l, SOURCE), node(l, GRAIN)),
          player.makeGroupTail(node(l, GRAIN), node(l, EFFECT)))
    }
    player.sendBundle((player.makeFreeAll(0) +: osc).toArray, 0)
  }

  def getGroup(layer: Int, group: GroupName) =
    layers(layer)(group)
}

case class Music(pulse: Pulse) extends Playable {

  def play()(implicit player: MusicPlayer) {
    val layers = Layers(pulse.tracks)
    layers.play()
    pulse.internalPlay(0, 0)(player, layers)
  }
}

object Music {
  val music = Music(Pulse())

  val player: MusicPlayer = MusicPlayer()
  val plotter: DataPlotter = DataPlotter()

  def play() = player.play(music)
}