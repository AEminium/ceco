package pt.uc.dei.cehm

class TryCatch(code: () => Unit) {
  def _catch[E <: Exception](handler:Function[E,Unit])(implicit m:Manifest[E]) = {
    new TryCatchExecuter[E](code, handler, m)._finally{}
  }

}

class TryCatchFinally(code: () => Unit) {
  def _catch[E <: Exception](handler:Function[E,Unit])(implicit m:Manifest[E]) = {
    new TryCatchExecuter[E](code, handler, m)
  }
}


class TryCatchExecuter[E <: Exception]
      (code: () => Unit, catcher: E => Unit, m:Manifest[E]) {
  def _finally(fin: => Unit) {
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