

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import java.security.MessageDigest
import akka.actor.ActorRef
import akka.routing.RoundRobinRouter


case object Calculate
case class Work(noOfZeros: Int)

class Worker(length: Int) extends Actor {

  def receive = {

    case Work(k) =>
     // println("work")
      calculateBitcoin(k)
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

  def calculateBitcoin(noOfZeros: Int) {
   // println("started calulating bit coin")

    val start = System.currentTimeMillis();
   // println(start)
    val b = "%0" + noOfZeros + "d"
    val z = b.format(0)
    var iter=0
    while (iter < 1000000) {
      val hashMe = "pavannelakuditi" + randomString(length)
      iter+=1
      val sha256 = cryptographicHash(hashMe)

      if (sha256.substring(0, noOfZeros).equals(z)) {
        println(hashMe + "  " + sha256)

      }
    }
    sender ! "Close"
  }
}

class Master(k: Int, a: Int) extends Actor {
  def noOfActors = a
  def noOfZeros = k
  var stop = 0
 // val workers = new Array[ActorRef](noOfActors)
   val worker = context.actorOf(Props(new Worker(20)).withRouter(RoundRobinRouter(noOfActors)), name = "worker")
  //for (i <- 0 until a) {
    //println("intialization of actors")
    //workers(i) = context.actorOf(Props(new Worker(20)), name = "actor" + "%s".format(i))
  //}

  def receive = {

    case "Calculate" =>
     // println("started calculate")
     val message_count = 20
     for (i <- 1 to message_count) {
    // println("message no : "+i)
      worker ! Work(noOfZeros)
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

object project1 {
  def main(args: Array[String]) {
    val k = if (args.length > 0) args(0).toInt else 4
    val noOfActors = if (args.length > 1) args(1).toInt else 8

    val system = ActorSystem("BitCoinSystem")
    val master = system.actorOf(Props(new Master(k, noOfActors)), name = "master")
    val start: Long = System.currentTimeMillis
   // println("started")
    master ! "Calculate"

  }

}
