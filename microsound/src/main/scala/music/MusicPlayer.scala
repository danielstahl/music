package music

import com.illposed.osc.OSCPortOut
import java.net.InetAddress
import com.illposed.osc.OSCMessage
import com.illposed.osc.OSCPacket
import com.illposed.osc.OSCBundle
import java.util.Date
import com.illposed.osc

case class MusicPlayer() {
  val sender: OSCPortOut = new OSCPortOut(InetAddress.getLocalHost(), 57110)

  val clock: PlayerClock = PlayerClock()

  def play(playable: Playable): Unit = {
    clock.reset()
    playable.play()(this)
  }

  def play(playable: (MusicPlayer) => Unit): Unit = {
    play(
      new Playable {
        override def play()(implicit player: MusicPlayer): Unit = playable(player)
      }
    )
  }

  def sendNew(arguments: Seq[Object], deltaTime: Long)(implicit player: MusicPlayer) = {
    //println(s"Sending message at $deltaTime with args $arguments")
    val theMessages: Array[Object] = arguments.toArray
    sendBundle(Array(new OSCMessage("/s_new", theMessages)), deltaTime)(player)
  }

  /**
   * Add node to head of group
   */

  def makeGroupHead(groupId: Integer, nodeId: Integer) = {
    val theArguments: Array[Object] = Array(nodeId, new Integer(0), groupId)
    //println(s"making /g_new with arguments ${theArguments.toList}")
    new OSCMessage("/g_new", theArguments)
  }

  /**
   * Add node to tail of group
   */

  def makeGroupTail(groupId: Integer, nodeId: Integer) = {
    val theArguments: Array[Object] = Array(nodeId, new Integer(1), groupId)
    //println(s"making /g_new with arguments ${theArguments.toList}")
    new OSCMessage("/g_new", theArguments)
  }

  def makeFreeAll(nodeId: Integer) = {
    val theArguments: Array[Object] = Array(nodeId)
    new OSCMessage("/g_freeAll", theArguments)
  }

  def sendBundle(messages: Array[OSCPacket], deltaTime: Long)(implicit player: MusicPlayer) = {
    println(s"sending ${messages.toList} at $deltaTime")
    val theTime = new Date(player.clock.start + deltaTime)
    player.sender.send(new OSCBundle(messages, theTime))
  }
}

case class PlayerClock() {
  val DELAY: Long = 1000
  var start: Long = System.currentTimeMillis() + DELAY

  def reset() = start = System.currentTimeMillis() + DELAY
}

trait Playable {
  def play()(implicit player: MusicPlayer)
}