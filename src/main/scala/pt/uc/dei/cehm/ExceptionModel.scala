package pt.uc.dei.cehm

import scala.collection.mutable.{Queue, SynchronizedQueue,HashMap}
import scala.actors.Actor
import scala.actors.OutputChannel
import scala.actors.Actor._

trait ExceptionModel extends Actor {
  
  val exceptionQueue:Queue[Exception] = new SynchronizedQueue[Exception]()

  /* Overwrite TryCatch and TryCatchFinally Statements */
  def _tryF(code: => Unit) {
    val f:( () => Unit) = { () => code }
    new TryCatchFinally(this, f)
  }
  
  def _try(code: => Unit) = {
    val f:( () => Unit) = { () => code }
    new TryCatch(this, f)
  }
  
  def _throw(e:Exception) {
    _raise(e)
    throw e
  }
  
  def _raise(e:Exception) {
    ExceptionController ! e
  }
  
  def _check = {
    if (!exceptionQueue.isEmpty) throw exceptionQueue.dequeue
  }
  
  def _receive(e:Exception) {
    exceptionQueue enqueue e
  }
  
}