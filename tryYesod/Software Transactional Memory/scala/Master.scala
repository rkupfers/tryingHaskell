package STM
import scala.actors.Actor
import scala.util.control.Breaks._
import scala.concurrent.stm._

class Master(number:Long) extends Actor {

  def act() {
    val t = System.nanoTime()
    val erg =Ref(0l)
      for(i<- 0l until number) {
        receive {
          case i: Long => atomic{ implicit txn => erg()=erg()+i}
          case _ => println("Fehler")
        }
      }
    atomic{implicit txn => println("Erg"+erg())}
    println("Time:"+(System.nanoTime()-t).toDouble/1000000000)
    
  }
 
}