package pt.uc.dei.examples

import scala.collection.immutable.List
import scala.actors.Actor
import scala.actors.Actor._
import pt.uc.dei.cehm._


case class Ack()
case class MessageNotSent(val msg:String) extends Exception {
  override def getMessage = msg
}

object App {
  def main(args : Array[String]) {
      messageController.start
  }
}

object messageController extends Actor with ExceptionModel {   
  def act() {
    println("[Controller] Starting to send messages..")
    
    // initialize the system
    ExceptionController.start

    def handle(e:Exception) = {
      println("[Controller] Exception caught: " + e.getMessage)
    }
    
    var futures:List[MessageSender] = List()
    
    (1 to 15).foreach { m =>
      val ms = new MessageSender
      ms.start
      ms ! m
      futures = ms :: futures
    }
    
    futures.foreach{ m => m !? Stop }
    
    // Stop the system
    ExceptionController ! Stop
  }
}

class MessageSender extends Actor with ExceptionModel {   
  def work(i:Integer) {
    _try {
      if (i == 3) {
        Thread.sleep(5*1000)
        _throw (new Exception("Hello"))
      } 
      else {
        println("Message " + i + " being sent.")
        Thread.sleep(10*1000)
        _check
        println("Message " + i + " sent.")
      }
    } _catch {
      e:Exception => println("Got exception: " + e)
    }
  }
  
  def act() {
    loop {
      react {
        case Stop => {
          sender ! new Ack
          exit()
        }
        case i:Int => {
          work(i)
        }
        case _ => ()
      }
    }
  }
}