package pt.uc.dei.cehm

class TryCatch(code: () => Unit) {
  
  def _catch(handler:Function[Exception,Unit]) = {
    new TryCatchFinally(code)._catch(handler)._finally{}
  }

}

class TryCatchFinally(code: () => Unit) {
  var catcher:Function[Exception,Unit] = null;
  def _catch(handler:Function[Exception,Unit]) = {
    catcher = handler
    this
  }
  
  def _finally(fin: => Unit) {
    try {
      ExceptionController !? new Register[Exception]
      code()
    } catch {
      case e : Exception => catcher(e)
    } finally {
      ExceptionController !? new Unregister[Exception]()
      fin
    }
  }
}