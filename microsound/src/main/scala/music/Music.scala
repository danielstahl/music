package music

import music._
import Harmony._
import com.illposed.osc.OSCPacket
import com.illposed.osc.OSCMessage
import com.illposed.osc.OSCPortOut
import java.util.Date
import com.illposed.osc.OSCBundle
import java.net.InetAddress
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


object Music {
  val player: MusicPlayer = MusicPlayer()
  val plotter: DataPlotter = DataPlotter()  
}