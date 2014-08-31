package STM
import scala.actors.Actor

class Worker(func:(Long,Long)=>Long,von:Long,bis:Long,parent:Actor) extends Actor{
  
	def act(){
	  parent!func(von,bis)
	}
}