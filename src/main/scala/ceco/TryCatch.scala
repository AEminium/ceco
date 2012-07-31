package ceco

import scala.reflect.{ Manifest, ClassManifest }

trait TryCatchDispatcher {
  var async = false;

  def dispatch[E <: Exception](actor: ExceptionModel, code: () => Unit, handler: Function[E, Unit], m: Manifest[E]): TryCatchExecuter[E] = {
    if (m <:< ClassManifest.fromClass(classOf[ConcurrentException])) {
      new TryCatchRemoteExecuter[E](actor, code, handler, async, m)
    } else {
      new TryCatchExecuter[E](code, handler, m)
    }
  }
}

class TryCatchFinally(val actor: ExceptionModel, code: () => Unit) extends TryCatchDispatcher {
  def _catch[E <: Exception](handler: Function[E, Unit])(implicit m: Manifest[E]) = {
    dispatch[E](actor, code, handler, m)
  }
}

class TryCatch(val actor: ExceptionModel, code: () => Unit) extends TryCatchDispatcher {
  def _catch[E <: Exception](handler: Function[E, Unit])(implicit m: Manifest[E]): Unit = {
    dispatch[E](actor, code, handler, m)._finally {}
  }
}

class AsyncTryCatch(val actor: ExceptionModel, code: () => Unit) extends TryCatchDispatcher {
  def _catch[E <: Exception](handler: Function[E, Unit])(implicit m: Manifest[E]): Unit = {
    async = true;
    dispatch[E](actor, code, handler, m)._finally {}
  }
}

class AsyncTryCatchFinally(val actor: ExceptionModel, code: () => Unit) extends TryCatchDispatcher {
  def _catch[E <: Exception](handler: Function[E, Unit])(implicit m: Manifest[E]) = {
    async = true;
    dispatch[E](actor, code, handler, m)
  }
}

class TryCatchExecuter[E <: Exception](code: () => Unit, catcher: E => Unit, m: Manifest[E]) {
  def _finally(fin: => Unit) {
    try {
      code()
    } catch {
      case e: E => catcher(e)
    } finally {
      fin
    }
  }
}

class TryCatchRemoteExecuter[E <: Exception](actor: ExceptionModel, code: () => Unit, catcher: E => Unit, async: Boolean, m: Manifest[E]) extends TryCatchExecuter[E](code, catcher, m) {

  def dispatchException(e: Exception) {
    e match {
      case e: ConcurrentException => e match {
        case e: E => catcher(e)
        case _ => {
          ExceptionController !? new Unregister[E](m)
          throw e
        }
      }
      case e: Exception => {
        ExceptionController !? new Unregister[E](m)
        throw e
      }
    }
  }

  override def _finally(fin: => Unit) {
    if (async)
      async_finally(fin)
    else
      sync_finally(fin)
  }

  def async_finally(fin: => Unit) {
    val t = new Thread(new Runnable {
      override def run() = {
        try {
          code()
          Thread.sleep(1000);
        } catch {
          case e: InterruptedException => { println("wrong") }
          case e: Exception => dispatchException(e)
        }
      }
    })
    ExceptionController !? new RegisterThread[E](t, m)
    t.start
    t.join
    checkAllExceptions
    ExceptionController !? new Unregister[E](m)
    fin
  }

  def sync_finally(fin: => Unit) {
    try {
      ExceptionController !? new Register[E](m)
      code()
    } catch {
      case e: Exception => dispatchException(e)
    } finally {
      // implitic check after catches.
      checkAllExceptions
      ExceptionController !? new Unregister[E](m)
      fin
    }
  }

  def checkAllExceptions = {
    while (!actor.exceptionQueue.isEmpty) {
      try {
        actor._check
      } catch {
        case e: Exception => dispatchException(e)
      }
    }
  }
}
