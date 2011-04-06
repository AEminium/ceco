package pt.uc.dei.cehm

import scala.reflect.{Manifest, ClassManifest}

trait TryCatchDispatcher {
  def dispatch[E <: Exception](actor:ExceptionModel,code:() => Unit, handler:Function[E,Unit], m:Manifest[E]):TryCatchExecuter[E] = {
    if (m <:< ClassManifest.fromClass(classOf[RemoteException])) {
      new TryCatchRemoteExecuter[E](actor, code, handler, m)
    } else {
      new TryCatchExecuter[E](code, handler, m)
    }
  }
}

class TryCatchFinally(val actor:ExceptionModel, code: () => Unit) extends TryCatchDispatcher {
  def _catch[E <: Exception](handler:Function[E,Unit])(implicit m:Manifest[E]) = {
    dispatch[E](actor, code, handler, m)
  }
}

class TryCatch(val actor:ExceptionModel, code: () => Unit) extends TryCatchDispatcher{
  def _catch[E <: Exception](handler:Function[E,Unit])(implicit m:Manifest[E]):Unit = {
    dispatch[E](actor, code, handler, m)._finally {}
  }
}


class TryCatchExecuter[E <: Exception]
      (code: () => Unit, catcher: E => Unit, m:Manifest[E]) {
  def _finally(fin: => Unit) {
    try {
      code()
    } catch {
      case e:E => catcher(e)
    } finally {
      fin
    }
  }
}

class TryCatchRemoteExecuter[E <: Exception]
      (actor:ExceptionModel, code: () => Unit, catcher: E => Unit, m:Manifest[E]) extends TryCatchExecuter[E](code, catcher, m) {
        
  def dispatchException(e:Exception) {
    e match {
      case e:RemoteException => e match {
          case e:E => catcher(e)
          case _ => {
            ExceptionController !? new Unregister[E](m)
            throw e
          }
      }
      case e:Exception => {
        ExceptionController !? new Unregister[E](m)
        throw e
      }
    }
  }
        
  override def _finally(fin: => Unit) {
    try {
      ExceptionController !? new Register[E](m)
      code()
    } catch {
      case e:Exception => dispatchException(e)
    } finally {
      // implitic check after catches.
      while (!actor.exceptionQueue.isEmpty) {
       try {
         actor._check
       } catch {
         case e:Exception => dispatchException(e)
       }
      }
      ExceptionController !? new Unregister[E](m)
      fin
    }
  }
}
