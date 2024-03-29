import akka.actor._
import com.typesafe.config.ConfigFactory
import java.security.MessageDigest
import akka.routing.RoundRobinRouter
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import com.typesafe.config.ConfigFactory
import java.net.InetAddress
import scala.util.control.Breaks

case class startClient()
case class start(kval :Int)
case class Work(noOfZeros: Int,length: Int)
case class results(res : ArrayBuffer[String])
case class results1(res : String)

object Server{

 def main(args: Array[String]) {

	val k = if (args.length > 0) args(0).toInt else 4
    val noOfActors = if (args.length > 1) args(1).toInt else 2
    
    val hostname = InetAddress.getLocalHost.getHostName
    val config = ConfigFactory.parseString(
      """ 
     akka{ 
    		actor{ 
    			provider = "akka.remote.RemoteActorRefProvider" 
    		} 
    		remote{ 
                enabled-transports = ["akka.remote.netty.tcp"] 
            netty.tcp{ 
    			hostname = "127.0.0.1" 
    			port = 5151 
    		} 
      }    
       log-dead-letters-during-shutdown = on  
    }""")
	implicit val localSystem=ActorSystem("HelloServerSys",ConfigFactory.load(config))
	val remoteLiaison = localSystem.actorOf(Props(new RemoteLiaison(k)), name = "remoteLiaison")
	val master = localSystem.actorOf(Props(new Master(k,noOfActors)), name = "ServerMaster")
	master ! "Calculate" 
    remoteLiaison ! "start" 
//	remoteLiaison ! "close"
}
}
class RemoteLiaison(kval:Int) extends Actor{
		
		def receive = {
			case "start" => println("Remote Liaison up and running")
			case startClient() => 
				println("start the client")
				sender ! start(kval)
				println("send msg to client")
			case "close" => println("Remote Liaison shutting down")
						 context.system.shutdown()
			case results(res : ArrayBuffer[String]) =>
				println("From Client")
				for(i <- 0 until res.length) println(res(i))
			case results1(r : String) => 
				val e= r.split("   ")
				println("fake")
				for(i <- 0 until e.length) println(e(i))
		}
}


class Worker() extends Actor {

  def receive = {

    case Work(k,length) =>
     println("work")
      calculateBitcoin(k,length)
      sender ! "Close"
  }

  def randomString(length: Int) = {
    val r = new scala.util.Random
    val sb = new StringBuilder
    for (i <- 1 to length) {
      sb.append(r.nextPrintableChar)
    }

    sb.toString
  }

  def cryptographicHash(data: String): String =
    {
      val byteArray = MessageDigest.getInstance("SHA-256").digest(data.getBytes("UTF-8"))
      val hashedString = byteArray.map("%02x".format(_)).mkString
      return hashedString
    }

  def calculateBitcoin(noOfZeros: Int,length: Int) {
    println("started calulating bit coin")

    val start = System.currentTimeMillis();
   // println(start)
    val b = "%0" + noOfZeros + "d"
    val z = b.format(0)
    var iter=0
    while (true) {
   // println("in while loop")
      val hashMe = "pavannelakuditi" + randomString(length)
      iter+=1
      val sha256 = cryptographicHash(hashMe)

      if (sha256.substring(0, noOfZeros).equals(z)) {
        println(hashMe + "  " + sha256)

      }
    }
    sender ! "Close"
   // context.system.shutdown()
  }
}

class Master(k: Int, a: Int) extends Actor {
  def noOfActors = a
  def noOfZeros = k
  var stop = 0
  
  println("no of actors "+ a)
val worker = context.actorOf(Props(new Worker()).withRouter(RoundRobinRouter(noOfActors)), name = "worker")

  def receive = {

    case "Calculate" =>
     // println("started calculate")
      var i = 0
      val message_count = 2000
        for (i <- 1 to message_count) {
    // println("message no : "+i)
      worker ! Work(noOfZeros,20)
    }
    case "Close" =>
      stop += 1
      if (stop == a) {
       // println("Shutting system down")
        context.system.shutdown()
        System.exit(0)
      }
    // 

  }
}
