package pt.uc.dei.examples.email

import scala.collection.immutable.List
import scala.actors.Actor
import scala.actors.Actor._
import pt.uc.dei.cehm._

class AddressDoesNotExistException extends RemoteException
class IOException extends RemoteException


object MessageSending {
  def main(args : Array[String]) {
      messageController.start
  }
}

object messageController extends Actor with ExceptionModel {   
  def act() {
    println("[Controller] Starting to send messages..")
    
    // initialize the system
    ExceptionController.start
    
    var futures:List[MessageSender] = List()
    _try {
      (1 to 9).foreach { m =>
        val ms = new MessageSender
        ms.start
        ms ! m
        futures = ms :: futures
      }
      
      futures.foreach{ m => m !? Stop }
      _check
      
    } _catch {
      e:RemoteException => 
        e match {
            case e:AddressDoesNotExistException => println("[Controller] Address does not exist")
            case e:IOException => println("[Controller] IO error")
        }
    }
    
    // Stop the system
    ExceptionController ! Stop
  }
}

class MessageSender extends Actor with ExceptionModel {   
  def work(i:Integer) {
    _try {
      if (i == 3) {
        Thread.sleep(5*1000)
        _throw (new AddressDoesNotExistException)
      } else if ( i == 7) {
        Thread.sleep(5*1000)
        _throw (new IOException)
      }
      else {
        println("[Sender " + i + "] Message " + i + " being sent.")
        Thread.sleep(10*1000)
        _check
        println("[Sender " + i + "] Message " + i + " sent.")
      }
    } _catch { e:Exception => 
      println("[Sender " + i + "] Got local exception: " + e)
    }
  }
  
  def act() {
    loop {
      react {
        case Stop => {
          sender ! Ack
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