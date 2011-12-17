package examples.email

import scala.collection.immutable.List
import scala.actors.Actor
import scala.actors.Actor._

case object Stop
case object Ack
case object SendMessage

object PlainMessageSending {
	def main(args : Array[String]) {
		PlainMessageController.start
		PlainMessageController ! SendMessage
	}
}

object PlainMessageController extends Actor {   
	var database:List[String] = (0 to 8).map{ n => ("employee" + n + "@company.com") }.toList
	// Will store current sender actors
	var senderList:List[PlainMessageSender] = List()

	// Send to all addresses in database.
	var recipients:List[String] = database

	def act() {
		loop {
			react {
				case Stop => exit()
				case SendMessage => {
					println("[Controller] Starting to send messages..")
					if (recipients.length > 0) {
						senderList = List()
						recipients.zipWithIndex.foreach { m =>
							/* spawns a new PlainMessageSender actor for each email */
							val ms = new PlainMessageSender
							ms.start
							ms ! (m)
							senderList = ms :: senderList
						}
						/* Resets the recipient lists */
						recipients = List()

						/* Awaits for all senders to finish */
						senderList.foreach{ m => m !? Stop }
						this ! Stop
					}
				}
				case e:Exception => {
					e match {
						case e:InvalidAddressException => {
							/* If the address does not exist, remove the email from database  */
							database = database - e.email
							println("[Controller] " + e.email + " removed from database")
							exit()
						}
						case e:IOException => {
							/* In case of IO error, retry */
							recipients = e.email :: recipients
							this !! SendMessage
							println("[Controller] IO error")
						}
					}
				}
			}
		}
	}
}

/* 
MessageSender will send an email to a given address.
It may raise InvalidAddressException or IOException.

Behavior is faked. Each actor will await 10 minutes before returning.
Email #3 will raise an InvalidAddressException.
Email #7 will raise an IOException the first time it is sent.
*/
class PlainMessageSender extends Actor {
	private var fakeError:Boolean = true

	def work(i:Integer, e:String) {
		try {
			if (i == 3) {
				Thread.sleep(5*1000)
				if (PlainMessageController.getState == Actor.State.Terminated) PlainMessageController.restart
				PlainMessageController !! new InvalidAddressException(e)
			} else {
					if ( i == 7 && fakeError) {
						Thread.sleep(5*1000)
						fakeError = false
						// Same here, but with a different kind of exception
						if (PlainMessageController.getState == Actor.State.Terminated) PlainMessageController.restart
						PlainMessageController !! new IOException(e)
					}
					else {
						// Regular behavior.
						println("[Sender " + i + "] sending to " + e  + "...")
						Thread.sleep(10*1000)
						println("[Sender " + i + "] sent.")
					}
				}
				} catch { case e:Exception => 
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