package music

import scala.swing._
import scala.swing.event.Event
import java.awt.Font
import java.awt.RenderingHints

class PlottableComponent extends Component {	
	var plottable: Option[Plottable] = None
	
	override def paintComponent(g: Graphics2D) {
	  plottable match {
	    case p =>
	      g.setFont(new Font("Arial", Font.PLAIN, 9))
	      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
	      p.get.plot(g)
	  }	  
	}
	
	reactions += {
	  case e: PlottableEvent =>
	    plottable = Option(e.plottable)
	}
}

class PlotterFrame(gc: java.awt.GraphicsConfiguration = null) extends Frame(gc) {
  override def closeOperation() {
    this.visible = false
  }
}

case class DataPlotter() extends Reactor {
	var top: PlotterFrame = setupGui
	val publisher: Publisher = PlottablePublisher()
	
	def setupGui: PlotterFrame = {
		Swing.onEDT {
			top = new PlotterFrame {
			  title = "Dataplotter"
			  contents = new BorderPanel {				  
				  add(makeScrollPane(makeComponent(publisher)), BorderPanel.Position.Center)
			  }	
			  if(size == new Dimension(0,0)) {
			    pack()
			  }
			}
		}
	  top
	}
  
	def plot(plottable: Plottable) {	  
		Swing.onEDT {			
			top.visible = true
			publisher.publish(PlottableEvent(plottable))
		}
	}

  def plot(plotter: (Graphics2D) => Unit) {
    plot(
      new Plottable {
        override def plot(g: Graphics2D): Unit = plotter(g)
      }
    )
  }

	def makeScrollPane(component: Component): ScrollPane = {
	  new ScrollPane(component) {
	    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Always
	  }
	}
	
	def makeComponent(publisher: Publisher): PlottableComponent = {
	  new PlottableComponent {
	    preferredSize = new Dimension(7000, 1000)
	    listenTo(publisher)
	  }
	}
}

case class PlottableEvent(plottable: Plottable) extends Event

case class PlottablePublisher() extends Publisher


trait Plottable {
	def plot(g: Graphics2D)
}