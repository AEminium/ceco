package ceco

import scala.reflect.{Manifest, ClassManifest}
import scala.collection.mutable.{Map, HashMap}
import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.OutputChannel

case object Ack
case object Stop
case class Register[E <: Exception](m: Manifest[E]) {
  def getType = m
}
case class RegisterThread[E <: Exception](t:Thread, m: Manifest[E]) {
	def getThread = t
  def getType = m
}
case class Unregister[E <: Exception](m: Manifest[E]) {
  def getType = m
}

abstract class ExceptionMessenger(out:OutputChannel[Any]) {
	def getId = out.receiver
	def deliver(e:Exception):Unit
}

class SyncExceptionMessenger(out:OutputChannel[Any]) extends ExceptionMessenger(out) {
	override def deliver(e:Exception) {
		if (out.receiver.isInstanceOf[ExceptionModel]) {
      out.receiver.asInstanceOf[ExceptionModel]._receive(e)
    } else {
      println("All actors should use the ExceptionModel")
      System.exit(1)
    }
	}
}

class AsyncExceptionMessenger(out:OutputChannel[Any], t:Thread) extends SyncExceptionMessenger(out) {
	override def deliver(e:Exception) {
		super.deliver(e)
		t.interrupt
	}
}


object ExceptionController extends Actor {
  
	var shouldShutdown = false
  val registry:Map[Manifest[_], List[ExceptionMessenger]] = new HashMap;
  
  def act() {
    loop {
      react {
        case r:Register[_] => {
          register(sender, r.getType)
          sender ! Ack
        }
        case r:RegisterThread[_] => {
          register(sender, r.getThread, r.getType)
          sender ! Ack
        }
        case r:Unregister[_] => {
          unregister(sender, r.getType)
          sender ! Ack
					checkExit
        }
        case e:Exception => {
          this << e
        }
        case Stop => {
					shouldShutdown = true
					checkExit
				}
        case _ => System.err.println("[Warning] ExceptionController has just received garbage messages.")
      }
    }
  }

	def checkExit = {
		if (shouldShutdown && (registry.size == 0 || registry.values.map(_.length).sum == 0)) exit()
	}


  def register(a:OutputChannel[Any], t:Thread, manif:Manifest[_]):Unit = registerMessenger(new AsyncExceptionMessenger(a,t), manif)
  def register(a:OutputChannel[Any], manif:Manifest[_]):Unit = registerMessenger(new SyncExceptionMessenger(a), manif)

  def registerMessenger(k:ExceptionMessenger, manif:Manifest[_]) {
    val t = manif
    registry(t) = k :: (if (registry contains t) registry(t) else Nil)
  }
  
  def unregister(a:OutputChannel[Any], manif:Manifest[_]) = {
    val t = manif
		if (registry contains t) {
	    var b = true
	    registry(t) = registry(t).filterNot{ o => 
	        if (o.getId == a.receiver && b)  {
	          b = false
	          true
	        } else false
	    }
		}
  }
  
  def << (e:Exception) = {
    registry.keys.foreach { k =>
      if ( ClassManifest.singleType(e) <:< k ) {
        registry(k).distinct.foreach(_.deliver(e))
      }
    }
  }
}
