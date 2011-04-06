package pt.uc.dei.cehm

import scala.reflect.{Manifest, ClassManifest}
import scala.collection.mutable.{Map, HashMap}
import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.OutputChannel

case object Ack
case object Stop
case class Register[E <: Exception](m: Manifest[E]) {
  def getType() = m
}
case class Unregister[E <: Exception](m: Manifest[E]) {
  def getType() = m
}

case class Registration()

object ExceptionController extends Actor {
  
  val registry:Map[Manifest[_], List[OutputChannel[Any]]] = new HashMap;
  
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
    val t = manif
    registry(t) = a :: (if (registry contains t) registry(t) else Nil)
  }
  
  def unregister(a:OutputChannel[Any], manif:Manifest[_]) = {
    val t = manif
    registry(t) = registry(t).filter( o => o.receiver != a.receiver )
  }
  
  def << (e:Exception) = {
    registry.keys.foreach { k =>
      if ( ClassManifest.singleType(e) <:< k ) {
        registry(k).foreach { out =>
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
}
