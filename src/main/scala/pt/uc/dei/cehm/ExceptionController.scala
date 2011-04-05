package pt.uc.dei.cehm

import scala.collection.mutable.{Map, HashMap}
import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.OutputChannel

case object Ack
case object Stop
case class Register[E <: Exception ](implicit m: Manifest[E]) {
  def test(e:Exception):Boolean = e.isInstanceOf[E]
  def getType() = m
}
case class Unregister[E](implicit m: Manifest[E]) {
  def test(e:Exception):Boolean = e.isInstanceOf[E]
  def getType() = m
}

case class Registration()

object ExceptionController extends Actor {
  
  val registry:Map[String, List[OutputChannel[Any]]] = new HashMap;
  
  def act() {
    loop {
      react {
        case r:Register[_] => {
          register(sender, r.getType)
          sender ! Ack
        }
        case r:Unregister[_] => {
          unregister(sender, r.getType)
          sender ! Ack
        }
        case e:Exception => {
          this << e
        }
        case Stop => exit()
        case _ => println("Unknown option.")
      }
    }
  }

  def register(a:OutputChannel[Any], manif:Manifest[_]) = {
    val t = manif.toString
    registry(t) = a :: (if (registry contains t) registry(t) else Nil)
  }
  
  def unregister(a:OutputChannel[Any], manif:Manifest[_]) = {
    val t = manif.toString
    registry(t) = registry(t).filter( o => o.receiver != a.receiver )
  }
  
  def << (e:Exception)(implicit m: Manifest[Exception]) = {
    val t = m.toString
    if (registry contains t) {
      registry(t).foreach{ out =>
        if (out.receiver.isInstanceOf[ExceptionModel]) {
          out.receiver.asInstanceOf[ExceptionModel]._receive(e)
        } else {
          println("All actors should use the ExceptionModel")
          System.exit(1)
        }
      }
    }
  }
}
