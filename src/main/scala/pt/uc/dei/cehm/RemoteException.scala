package pt.uc.dei.cehm

case class RemoteException(val exception:Exception) extends Exception {
  def getException = exception
}