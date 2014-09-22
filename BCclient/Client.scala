
import akka.actor._
import com.typesafe.config.ConfigFactory
import java.net.InetAddress
import scala.collection.mutable.ArrayBuffer
import akka.routing.RoundRobinRouter
import java.security.MessageDigest
import scala.util.control.Breaks
import scala.util.Random


case class startClient()
case class start(kval :Int)
case class results(str : String)

object Remote extends App {	
   val hostname = InetAddress.getLocalHost.getHostName
   val config = ConfigFactory.parseString(
      """akka{
		  		actor{
		  			provider = "akka.remote.RemoteActorRefProvider"
		  		}
		  		remote{
                   enabled-transports = ["akka.remote.netty.tcp"]
		  			netty.tcp{
						hostname = "127.0.0.1"
						port = 2220
					}
				}     
    	}""")
	implicit val clientSystem = ActorSystem("HelloClientSys",ConfigFactory.load(config))
	val master = clientSystem.actorOf(Props(new Master(3)), name = "ClientMaster")
	
	master ! "askServer"
    


}



case class Work(noOfZeros: Int,remote : ActorRef)

class Worker(length: Int) extends Actor {

  def receive = {

    case Work(k,remote) =>
     // println("work")
      calculateBitcoin(k,remote)
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

  def calculateBitcoin(noOfZeros: Int,remote : ActorRef) {
   // println("started calulating bit coin")

    val start = System.currentTimeMillis();
   // println(start)
   var str="From CLient "
    val b = "%0" + noOfZeros + "d"
    val z = b.format(0)
    var iter=0
    while (iter < 1000000) {
      val hashMe = "pavannelakuditi" + randomString(length)
      iter+=1
      val sha256 = cryptographicHash(hashMe)

      if (sha256.substring(0, noOfZeros).equals(z)) {
        println(hashMe + "  " + sha256)
 		str+=hashMe + " " + sha256
      }
    }
    remote ! results(str)
    sender ! "Close"
  }
}

class Master(a: Int) extends Actor {
	val ip ="127.0.0.1"
	val remote = context.actorFor("akka.tcp://HelloServerSys@" + ip + ":5151/user/remoteLiaison")
  def noOfActors = a
  //def noOfZeros = k
  var stop = 0
  val workers = new Array[ActorRef](noOfActors)
  for (i <- 0 until a) {
    //println("intialization of actors")
    workers(i) = context.actorOf(Props(new Worker(20)), name = "actor" + "%s".format(i))
  }

  def receive = {
    case "askServer" => 
    println("requesting server permission")
    remote ! startClient()

    case start(noOfZeros : Int) =>
     println("started calculate")
      var i = 0
      while (i < a) {
        //println("in while loop " + i)
        workers(i) ! Work(noOfZeros,sender)
        i = i + 1
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
