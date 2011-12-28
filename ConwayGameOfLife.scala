

/**
 * The cellular automation known as ﻿Conway's Game of Life is a good example of how a few simple rules can produce interesting patterns.
 *  The Game of Life is a two dimensional world consisting of cells arranged in rows and columns, where each cell has exactly 8 neighbours.
 *  A cell can either be alive ( on ) or dead ( off ).
 *
 *  Starting from some initial configuration of the world, the cells evolve based on the following:
 *   a cell is born if it has exactly 3 alive neighbouring cells,
 *   a cell continues to live if it subsequently has 2 or 3 alive neighbouring cells,
 *   a cell dies from loneliness if it has less than 2, or from over congestion if it has more than 3, alive neighbouring cells
 *
 * For this puzzle, assume that the edges of the world are joined together so that the world wraps around;
 *  for example, in a 10x10 world, the neighbours of the cell at (x,y) == (0,0) are (9,1), (0,1), (1,1), (9,0), (1,0), (9,9), (0,9), (1,9).
 *
 * To get a feel for it, start by writing a Game of Life simulator that displays the generations of cells on the screen.
 * Use any size for the world you think is appropriate, a random world for the initial configuration,
 *  and an appropriate tool that helps display the results (e.g. Java2D for Java and Scala; HTML for JavaScript; PyGame for Python; etc).
 *
 * Now for the challenge. ConSIDEr a world of size MxM, and a simulation that runs for N generations.
 *  For a simple, straightforward implementation, the program completes the simulation in T seconds.
 *  Now improve your algorithm such that the simulation finishes in less than half the time (T/2 seconds), or if you are really confident,
 *  in less than a quarter of the time (T/4 seconds).
 *  For this part, you do not have to display the results on the screen (otherwise you might be limited by your graphics library).
 *
 * To ensure a consistent initial configuration, use the following to initialize the world:
 *
 * cells along the horizontal axis (y == 0) are on
 * cells along the vertical axis (x == 0) are on
 * cells along the diagonal (x == y) are on
 * all other cells are off
 * Different environments and programming languages will execute at different speeds,
 *  so use your own judgement when deciding on the size of the world and the number of iterations to generate.
 *  As an example, a simple C++ implementation using a 1000x1000 world running for 1000 iterations runs in around 50 seconds on my machine.
 *  An improved algorithm runs in 12 seconds.
 *
 * Implementation note:
 * How do we speed this up ?
 * 1) Use multiple threads to process a segment of the cube.
 * The tricky bit is working out the neighbors in the case of wrapping with segments.
 * A 2D space broken into 4 segments (quarters) will require information from other segments.
 * Can we have 4 threads work on the same workspace .. this may mean contention issues when writing,
 * Ideally you pass start and end coordinates to each thread to do the processing.
 *
 */

import scala.swing.Publisher
import Commonutils._
import scala.actors.Actor
import Controller._
import Constants._

class ConwayGameOfLife extends Actor {

  var cellsA = Array.ofDim[Int](SIDE, SIDE)
  var cellsB = Array.ofDim[Int](SIDE, SIDE)
  var originator: scala.actors.OutputChannel[Any] = null
  /**
   * cells along the horizontal axis (y == 0) are on
   * cells along the vertical axis (x == 0) are on
   * cells along the diagonal (x == y) are on
   * all other cells are off
   */
  def init(c: Array[Array[Int]]) = {
    for (i <- 0 until SIDE; j <- 0 until SIDE) {
      if (i == 0)
        c(i)(j) = 1
      else if (j == 0)
        c(i)(j) = 1
      else if (i == j)
        c(i)(j) = 1
      else
        c(i)(j) = 0
    }

    // TODO: Is there an easier way to initialise using maps ?
    //    cells.map( cell => cell.map( value => reset(value)) )    

  }

  /**
   * Need to support wraparound behavior
   * Given x,
   *  if ((x+1) == SIDE) then ((x+1) % SIDE)
   *  if ((x-1) < 0) then ((x-1)+SIDE)
   *
   *  Need to think of a higher order kind function that only walks the immediate neighbors of a cell
   *
   */
  def numberNeighbors(x: Int, y: Int, a: Array[Array[Int]]): Int = {
    var num: Int = 0

    val nx = if ((x - 1) < 0) (x - 1 + SIDE) else (x - 1)
    val ny = if ((y - 1) < 0) (y - 1 + SIDE) else (y - 1)
    val px = if ((x + 1) == SIDE) ((x + 1) % SIDE) else (x + 1)
    val py = if ((y + 1) == SIDE) ((y + 1) % SIDE) else (y + 1)

    num += a(x)(ny)
    num += a(x)(py)
    num += a(nx)(y)
    num += a(nx)(ny)
    num += a(nx)(py)
    num += a(px)(y)
    num += a(px)(ny)
    num += a(px)(py)

    //    println(x,y,nx,ny,px,py,num)
    num
  }

  /**
   *
   * Starting from some initial configuration of the world, the cells evolve based on the following
   *  a cell is born if it has exactly 3 alive neighbouring cells,
   *  a cell continues to live if it subsequently has 2 or 3 alive neighbouring cells,
   *  a cell dies from loneliness if it has less than 2, or from over congestion if it has more than 3, alive neighbouring cells
   */
  def grow(a: Array[Array[Int]], b: Array[Array[Int]]) {

    for (i <- 0 to SIDE - 1; j <- 0 to SIDE - 1) {

      // Need to find how many neighbors you have.
      val neighbors = numberNeighbors(i, j, a)

      // Apply the rules above
      if (a(i)(j) == 0) {
        /*
           * Cell if OFF
           */
        if (neighbors == 3)
          b(i)(j) = 1
      } else {
        /*
            * Cell is ON
            */
        if (neighbors > 3 || neighbors < 2)
          b(i)(j) = 0
      }

    }

  }

  /**
   * The implementation of the actor below is a little unusal because I wanted a way of pausing the
   * Conway algorithm from the UI. This proved to be quite difficult to do with the
   * actor library as is.
   * Hence the approach I am using here is to kickstart the algorithm after each iteration by sending myself a message.
   * This means that I am able to respond quite quickly to a "Stop" message from the UI.
   */
  def act() {
    var i = 0
    var stop = false
    var timer:Long = 0

    loop {
      react {
        case Start => {

          i += 1

          // If it is a "Start" request from the UI then reset the stop flag.
          if (sender == originator) {
            stop = false
            timer = System.currentTimeMillis()
          }

          // Apply the rules for growth
          grow(cellsA, cellsB)

          // Send the results to the UI
          originator ! Data(cellsB)

          // Update the original cube
          cellsA = deepcopy(cellsB)

          if ((i != MAXITERATIONS) && !stop) {
            // kickstart myself
            Actor.self ! Start
          }

          // Uncomment the line below to see the animation more clearly... 
          //Thread.sleep(20)
        }
        case Stop => stop = true; println("Time taken b/n start and stop= " + (System.currentTimeMillis() - timer))
        case Init =>
          // save away the id of the controller as the sender.
          originator = sender
          init(cellsA)
          cellsB = deepcopy(cellsA)
        //Array.copy(cells,0,cellsOri,0,cells.length); 
        //cellsOri = copy(cells)
        // output(cellsOri)

      }
    }
  }

}
