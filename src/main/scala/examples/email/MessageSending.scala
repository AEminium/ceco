/*
This example represents a batch sending of email messages.
For each email a new actor is created, benefitting of the parallelism,
and two exceptions may be raised, and caught by the MessageController:

* Invalid Address: if such address does not exist, it should be removed
from database.

* IOException: Something went wrong with the communication channels and
the MessageController should retry to send it.

*/
package examples.email

import scala.collection.immutable.List
import scala.actors.Actor
import scala.actors.Actor._
import ceco._


/* Both exceptions are ConcurrentExceptions  */
class InvalidAddressException(val email:String) extends ConcurrentException
class IOException(val email:String) extends ConcurrentException


object MessageSending {
  def main(args : Array[String]) {
      MessageController.start
  }
}

/*
MessageController is responsible for sending a message to each email
address on a database.
*/
object MessageController extends Actor with ExceptionModel {   
  def act() {
    println("[Controller] Starting to send messages..")
    
    // ExceptionController delivers exceptions to interested actors
    ExceptionController.start
    
    var database:List[String] = (0 to 8).map{ n => ("employee" + n + "@company.com") }.toList
    
    // Will store current sender actors
    var senders:List[MessageSender] = List()
    
    // Send to all addresses in database.
    var recipients:List[String] = database
    
    while (!recipients.isEmpty) {
      senders = List()
      
      // Concurrent try
      _try {
        recipients.zipWithIndex.foreach { m =>
          /* spawns a new MessageSender actor for each email */
          val ms = new MessageSender
          ms.start
          ms ! (m)
          senders = ms :: senders
        }
        /* Resets the recipient lists */
        recipients = List()
        
        /* Awaits for all senders to finish */
        senders.foreach{ m => m !? Stop }
        
        /*
          Verifies wether any exceptions were thrown by senders
          In this case it is important to have explicitly, in order
          to ask for all addresses first.
        */
        _check
    
      } _catch {
        /*
          We are interested in ConcurrentExceptions. 
        */
        e:ConcurrentException => 
          e match {
              case e:InvalidAddressException => {
                /* If the address does not exist, remove the email from database  */
                database = database - e.email
                println("[Controller] " + e.email + " removed from database")
              }
              case e:IOException => {
                /* In case of IO error, retry */
                recipients = e.email :: recipients
                println("[Controller] IO error")
              }
          }
      }
    }
    println("[Controller] Stopping")
    // No longer needed.
    ExceptionController ! Stop
  }
}


/* 
  MessageSender will send an email to a given address.
  It may raise InvalidAddressException or IOException.
  
  Behavior is faked. Each actor will await 10 minutes before returning.
  Email #3 will raise an InvalidAddressException.
  Email #7 will raise an IOException the first time it is sent.
*/
class MessageSender extends Actor with ExceptionModel {
  private var fakeError:Boolean = true
    
  def work(i:Integer, e:String) {
    /* Local, regular try. See _catch for details */
    _try {
      if (i == 3) {
        Thread.sleep(5*1000)
        
        // Raise the exception for the controller and terminate execution
        _throw (new InvalidAddressException(e))
      } else if ( i == 7 && fakeError) {
        Thread.sleep(5*1000)
        fakeError = false
        // Same here, but with a different kind of exception
        _throw (new IOException(e))
      }
      else {
        // Regular behavior.
        println("[Sender " + i + "] sending to " + e  + "...")
        Thread.sleep(10*1000)
        println("[Sender " + i + "] sent.")
      }
    } _catch { e:Exception => 
      /*
      When catching for regular Exception, all the regular behavior
      of a traditional try-catch is kept.
      */
      println("[Sender " + i + "] Got local exception: " + e)
    }
  }
  
  /* Actor message handling details. */
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