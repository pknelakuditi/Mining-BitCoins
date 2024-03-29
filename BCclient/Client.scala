
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
case class results(str : ArrayBuffer[String])
case class localMessage(res : String)
case class results1(res : String)

object Remote {

 def main(args: Array[String]) {	

val ip =if( args.length > 0)  args(0).toString else "127.0.0.1"

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
				log-dead-letters = 10
  log-dead-letters-during-shutdown = on  
    	}""")
	implicit val clientSystem = ActorSystem("HelloClientSys",ConfigFactory.load(config))
	
	val master = clientSystem.actorOf(Props(new Master(3,ip)), name = "ClientMaster")
	
	master ! "askServer"
    
}

}



case class Work(noOfZeros: Int,length: Int)

class Worker() extends Actor {

  def receive = {

    case Work(k,length) =>
     // println("work")
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
   // println("started calulating bit coin")

   // val start = System.currentTimeMillis();
   // println(start)
   var str="From CLient "
    val b = "%0" + noOfZeros + "d"
    val z = b.format(0)
    var iter=0
    var result =""
    while (iter < 200000) {
      val hashMe = "pavannelakuditi" + randomString(length)
      iter+=1
      val sha256 = cryptographicHash(hashMe)
      
      if (sha256.substring(0, noOfZeros).equals(z)) {
        println(hashMe + "  " + sha256)
 		result=hashMe + " " + sha256+"   "
 		//println("sending local msg")
 		sender ! localMessage(result)
      }
    }
   // remote ! results(str)
    //sender ! "Close"
  
  }
}

class Master(a: Int,ip : String) extends Actor {
	
	val remote = context.actorFor("akka.tcp://HelloServerSys@" + ip + ":5151/user/remoteLiaison")
  def noOfActors = a
  //def noOfZeros = k
  var stop = 0
 val worker = context.actorOf(Props(new Worker()).withRouter(RoundRobinRouter(noOfActors)), name = "worker")
 var result: ArrayBuffer[String] = new ArrayBuffer[String]();
  val message_count = 50

  def receive = {
    case "askServer" => 
    println("requesting server permission")
    remote ! startClient()

    case start(noOfZeros : Int) =>
     println("started calculate")
       
        for (i <- 1 to message_count) {
    // println("message no : "+i)
      worker ! Work(noOfZeros,25)
    }
    case localMessage(res : String) => {
     	//	println("success local msg")
       result+=res
    }
    case "Close" =>
      stop += 1
      println("No of messages : "+stop)
      if (stop == message_count) {
   //   println("sending results")
      var re="From client  "
      var resflag=0
      for(i <- 0 until result.length){
      re+=result(i) 
      resflag+=1
      if(resflag==50 || i==result.length-1){
     //  println(re)
       remote ! results1(re)
        Thread.sleep(3000L);

       re ="next From client   "
       resflag=0
      }          
      } 
      
     //   remote ! results(result)
        println("Shutting system down")
        //context.system.shutdown()
        context.stop(self)
        System.exit(0)
      }
    // 

  }
}
