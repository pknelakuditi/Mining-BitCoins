import akka.actor._
import com.typesafe.config.ConfigFactory
import java.security.MessageDigest
import akka.routing.RoundRobinRouter
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import com.typesafe.config.ConfigFactory
import java.net.InetAddress
import scala.util.control.Breaks


object Server extends App{
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
    			hostname = "localhost" 
    			port = 5150 
    		} 
      }      
    }""")
	implicit val localSystem=ActorSystem("HelloServerSys",ConfigFactory.load(config))
	//val remoteLiaison = system.actorOf(Props(new RemoteLiaison(4)), name = "remoteLiaison")
	    val remoteLiaison = localSystem.actorOf(Props(new RemoteLiaison(4)), name = "remoteLiaison")
     remoteLiaison ! "start" 
	remoteLiaison ! "close"
}

class RemoteLiaison(kval:Int) extends Actor{
		
		def receive = {
			case "start" => println("Remote Liaison up and running")
			case "close" => println("Remote Liaison shutting down")
						 context.system.shutdown()
		}
}

class ServerMaster(kval:Int) extends Actor{
		
		def receive = {
			case "startmining" => {
			
			}
			case results(ArrayBuffer(String))
			case "close" => println("Mining in Server is shutting down")
						 context.system.shutdown()
		}
}
