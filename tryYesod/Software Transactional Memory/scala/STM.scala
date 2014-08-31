package STM

object STM {

  def bitcount(zahl: Long): Long =
    zahl match {
      case 0 => 0
      case _ => if (zahl % 2 == 0) bitcount(zahl / 2)
      			else bitcount(zahl / 2) + 1
    }

  def count(von: Long, bis: Long): Long = {

    return (von to bis).foldLeft(0l)(_+bitcount(_))
  }
  
  def main(args: Array[String]): Unit = {
    val k = 1<<26l
    val worker =8l
	val master = new Master(worker);
	master.start
	for(i<-1l until worker){
		new Worker(count,(i-1l)*k/worker,(i*k/worker)-1l,master).start
	}
	new Worker(count,(worker-1l)*k/worker,(worker*k/worker),master).start
  }

}