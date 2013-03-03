
package com.bdr.puzzle.conway

import scala.swing.Swing._
import scala.swing.{ MainFrame, Panel }
import java.awt.{ Color, Graphics2D, Point, geom }
import scala.swing.SimpleSwingApplication
import scala.swing.Button
import scala.swing.event.ButtonClicked
import scala.swing.FlowPanel
import Commonutils._
import scala.swing.BoxPanel
import scala.swing.Orientation
import Constants._

/**
 * UI for the Conway Game of Life.
 */
object ConwayGameOfLifeUI extends SimpleSwingApplication {

  lazy val ui = new Panel {

    background = Color.white
    preferredSize = (PANELSIZE, PANELSIZE)
    focusable = true

    val offColor = java.awt.Color.WHITE;
    val onColor = java.awt.Color.LIGHT_GRAY;

    var cells = Array.ofDim[Int](SIDE, SIDE)
    val cellwidth = (PANELSIZE / cells.length);

    override def paintComponent(g: Graphics2D) = {

      super.paintComponent(g)

      for (i <- 0 until cells.length; j <- 0 until cells.length)
        if (cells(i)(j) == 1) {
          g.drawRect(i * cellwidth, j * cellwidth, cellwidth, cellwidth)
          g.setColor(onColor)
          g.fillRect(i * cellwidth, j * cellwidth, cellwidth, cellwidth)
        } else {
          g.drawRect(i * cellwidth, j * cellwidth, cellwidth, cellwidth)
          g.setColor(offColor)
          g.fillRect(i * cellwidth, j * cellwidth, cellwidth, cellwidth)
        }

    }

    def setData(values: Array[Array[Int]]) = {
      cells = deepcopy(values)
      repaint()
    }
  }

  def top = new MainFrame {
    title = "Conway Game Of Life"

    var start = false
    val startButton = new Button("Start")
    val terminateButton = new Button("Terminate")

    val application = new ConwayGameOfLife().start()
    implicit val controller = Controller(application)

    listenTo(controller, startButton, terminateButton)

    // Kickstart the algorithm when "Start" button is pressed.
    Send(Init)

    reactions += {
      case ButtonClicked(`startButton`) => {
        if (!start) {
        	Send(Start)
        	startButton.text = "Stop"
        } else  {
        	Send(Stop)
        	startButton.text = "Start" 
        }
        // Toggle the start flag
        start = !start
      }
      case ButtonClicked(`terminateButton`) => exit()
      case Receive(Data(cells)) => ui.setData(cells)
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += ui
      contents += new FlowPanel(startButton, terminateButton)
    }
  }
}
