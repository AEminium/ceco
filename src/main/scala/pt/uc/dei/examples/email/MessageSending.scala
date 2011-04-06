package pt.uc.dei.examples.email

import scala.collection.immutable.List
import scala.actors.Actor
import scala.actors.Actor._
import pt.uc.dei.cehm._

class AddressDoesNotExistException(val email:String) extends RemoteException
class IOException(val email:String) extends RemoteException


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
    
    var senders:List[MessageSender] = List()
    
    var database:List[String] = (0 to 8).map{ n => ("employee" + n + "@company.com") }.toList
    var recipients:List[String] = database
    
    while (!recipients.isEmpty) {
      senders = List()
      _try {
        recipients.zipWithIndex.foreach { m =>
          val ms = new MessageSender
          ms.start
          ms ! (m)
          senders = ms :: senders
        }
        recipients = List()
    
        senders.foreach{ m => m !? Stop }
        _check
    
      } _catch {
        e:RemoteException => 
          e match {
              case e:AddressDoesNotExistException => {
                database = database - e.email
                println("[Controller] " + e.email + " does not exist")
              }
              case e:IOException => {
                recipients = e.email :: recipients
                println("[Controller] IO error")
              }
          }
      }
    }
    
    // Stop the system
    ExceptionController ! Stop
  }
}

class MessageSender extends Actor with ExceptionModel {
  private var fakeError:Boolean = true
    
  def work(i:Integer, e:String) {
    _try {
      if (i == 3) {
        Thread.sleep(5*1000)
        _throw (new AddressDoesNotExistException(e))
      } else if ( i == 7 && fakeError) {
        Thread.sleep(5*1000)
        fakeError = false
        _throw (new IOException(e))
      }
      else {
        println("[Sender " + i + "] sending to " + e  + "...")
        Thread.sleep(10*1000)
        _check
        println("[Sender " + i + "] sent.")
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
        case i:Tuple2[String, Int] => {
          work(i._2, i._1)
        }
        case i:Any => ()
      }
    }
  }
}