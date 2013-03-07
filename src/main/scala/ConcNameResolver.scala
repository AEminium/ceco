
import scala.actors._
import scala.actors.Actor._
import java.net._
import ceco._

class InvalidHostException(val url: String) extends ConcurrentException

class Controller extends Actor with ExceptionModel {

  def act() {

    println("[Controller] Starting...")

    // ExceptionController delivers exceptions to interested actors
    ExceptionController.start

    var w = new Worker
    w.start

    // colocar um contador de domain names para haver trocas de mensagens entre actores

    // Concurrent try
    _try {

      w ! ("www.dei.uc.pt", self)
      w ! ("wwww.dei.uc.pt", self)
      w ! ("www.mat.uc.pt", self)
      /*
			w ! ("www.darq.uc.pt", self)		
			w ! ("www.deec.uc.pt", self)		
			w ! ("www.fis.uc.pt", self)
			w ! ("www.mat.uc.pt", self)
			w ! ("www.dquimica.uc.pt", self)		
			w ! ("www.dcv.uc.pt", self)
			w ! ("www.deq.uc.pt", self)
			*/
      /*
			Verifies wether any exceptions were thrown by senders
			In this case it is important to have explicitly, in order
			to ask for all addresses first.
			*/
      _check

      Thread.sleep(10 * 1000)

    } _catch {
      /*
				We are interested in ConcurrentExceptions. 
				*/
      e: ConcurrentException ⇒
        e match {
          case e: InvalidHostException ⇒ {
            println("[Controller] InvalidHostException: " + e.url)
          }
        }
    }

    // No longer needed.
    ExceptionController ! Stop

    println("[Controller] Stopping.")
    exit()

  }
}

class Worker extends Actor with ExceptionModel {

  def act() {
    println("[Worker] Starting...")
    loop {
      react {
        case (name: String, actor: Actor) ⇒ {
          println("[Worker act]  name: " + name)
          actor ! getIp2(name)
          //act()
        }
        case "EXIT" ⇒
          println("[Worker act] Worker exiting.")
          // quit
          exit()
        case msg ⇒
          println("[Worker act] Unhandled message: " + msg)
        //act()
      }
    }
  }

  def getIp3(name: String): Option[String] = {
    var addr: InetAddress = null
    var ip: String = null

    Console.println("[Worker getIp2] name: " + name)

    _try {

      println("[Worker getIp2] Trying")

      try {
        addr = InetAddress.getByName(name)
      } catch {
        case _: Exception ⇒ {
          println("[Worker getIp2] Local Exception ")
          _throw(new InvalidHostException(name))
        }
      }

      if (addr != null) {
        Console.println("[Worker getIp2] addr: " + addr.toString)
        var ArrayAddr = addr.toString.split("/")
        ip = ArrayAddr(1)
        Console.println("[Worker getIp2] ip: " + ip)
      }

      Some(ip)

    } _catch {
      e: Exception ⇒ None
    }
  }

  def getIp2(name: String) = {

    var addr: InetAddress = null

    Console.println("[Worker getIp2] name: " + name)

    _try {

      println("[Worker getIp2] Trying...")

      try {
        addr = InetAddress.getByName(name)
      } catch {
        case _: Exception ⇒ {
          println("[Worker getIp2] Local Exception ")
          _throw(new InvalidHostException(name))
        }
      }

    } _catch {
      e: Exception ⇒ ()
    }

    if (addr != null) {
      Console.println("[Worker getIp2] addr: " + addr.toString)
      var ArrayAddr = addr.toString.split("/")
      var ip = ArrayAddr(1)
      Console.println("[Worker getIp2] ip: " + ip)
    }

  }

  def getIp(name: String): Option[String] = {

    var addr: InetAddress = null

    try {
      Console.println("[Worker getIp] name: " + name)
      addr = InetAddress.getByName(name)
      Console.println("[Worker getIp] addr: " + addr.toString)
      var ArrayAddr = addr.toString.split("/")
      var ip = ArrayAddr(1)
      Console.println("[Worker getIp] ip: " + ip)
      Some(ip)
    } catch {
      case _: UnknownHostException ⇒ {
        Console.println("[Worker getIp] Exception Invalid Host")
        throw (new InvalidHostException(name))
        None
      }
    }
  }
}

object ConcNameResolver {
  def main(args: Array[String]): Unit = {
    var c = new Controller
    c.start
  }
}