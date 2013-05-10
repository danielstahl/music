package music

import com.illposed.osc.OSCPortOut
import java.net.InetAddress
import com.illposed.osc.OSCMessage
import com.illposed.osc.OSCPacket
import com.illposed.osc.OSCBundle
import java.util.Date

case class MusicPlayer() {
  
  val sender: OSCPortOut = new OSCPortOut(InetAddress.getLocalHost(), 57110)

  val clock: PlayerClock = PlayerClock()

  def play(playable: Playable) = {
    clock.reset()
    playable.play()(this)
  }

  //def play(f: MusicPlayer => Unit) = {
  //  clock.reset()
  //  f(this)
  //}
  
  def sendNew(arguments: Seq[Object], deltaTime: Long)(implicit player: MusicPlayer) = {
    val theMessages: Array[Object] = arguments.toArray
    sendBundle(Array(new OSCMessage("/s_new", arguments.toArray)), deltaTime)(player)
  }

  def sendBundle(messages: Array[OSCPacket], deltaTime: Long)(implicit player: MusicPlayer) = {
    val theTime = new Date(player.clock.start + deltaTime)
    player.sender.send(new OSCBundle(messages, new Date(player.clock.start + deltaTime)))
  }
}

case class PlayerClock {
  val DELAY: Long = 1000
  var start: Long = System.currentTimeMillis() + DELAY
  
  def reset() = {
    start = System.currentTimeMillis() + DELAY
  }
}

trait Playable {
  def play()(implicit player: MusicPlayer) 
}