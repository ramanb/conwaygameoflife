
package com.bdr.puzzle.conway

import actors.Actor
import swing.Publisher
import swing.event.Event
import swing.Swing.onEDT

/**
 * Wrap up the communication between the worker thread(actor) and the UI thread.
 * The UI uses "Send(evt)" to notify the worker thread.
 * The worker thread (actor) uses "publish" to notify the UI. 
 */

case class Start 
case class Init
case class Stop
case class Data(val values: Array[Array[Int]]) 

case class Send(event: Any)(implicit controller: Controller) {
  controller ! this
}

case class Receive(event: Any) extends Event

case class Controller(application: Actor) extends Actor with Publisher {
  start()

  def act() {
    loop {(
      react ({
        case Send(evt) => application ! evt
        case evt => onEDT(publish(Receive(evt)))
      }))
    }
  }
}