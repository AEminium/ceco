package pt.uc.dei.cehm

trait TryCatchDispatcher {
  def dispatch[E <: Exception](code:() => Unit, handler:Function[E,Unit], m:Manifest[E]):TryCatchExecuter[E] = {
    if (m.erasure.isInstanceOf[RemoteException]) {
      new TryCatchRemoteExecuter[E](code, handler, m)
    } else {
      new TryCatchExecuter[E](code, handler, m)
    }
  }
}

class TryCatchFinally(code: () => Unit) extends TryCatchDispatcher {
  def _catch[E <: Exception](handler:Function[E,Unit])(implicit m:Manifest[E]) = {
    dispatch[E](code, handler, m)
  }
}

class TryCatch(code: () => Unit) extends TryCatchDispatcher{
  def _catch[E <: Exception](handler:Function[E,Unit])(implicit m:Manifest[E]):Unit = {
    dispatch[E](code, handler, m)._finally {}
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
      (code: () => Unit, catcher: E => Unit, m:Manifest[E]) extends TryCatchExecuter[E](code, catcher, m) {
  override def _finally(fin: => Unit) {
    try {
      ExceptionController !? new Register[E](m)
      code()
    } catch {
      case e:RemoteException => {
        e match {
            case e:E => catcher(e)
            case _ => throw e
        }
      }
      case e:Exception => {
        throw e
      }
    } finally {
      ExceptionController !? new Unregister[E](m)
      fin
    }
  }
}
