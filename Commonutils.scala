
/**
 * Utility methods.
 */
object Commonutils {

  def swap[A, B](a: A, b: B): (B, A) = (b, a)

  /**
   * Helper for printing out the cell values
   */
  def output(c: Array[Array[Int]]) = {
    println
    c.foreach {
      cell =>
        cell.foreach {
          value => print(value)
        }
        println
    }
  }

  /**
   * Make a deep copy of A
   * TODO: There must be a simpler way of getting a deep copy in Scala.
   */
  def deepcopy[A](a: A)(implicit m: reflect.Manifest[A]): A =
    util.Marshal.load[A](util.Marshal.dump(a))

  /**
   * Time a block of code
   */
  def timed[R](blockName: String)(block: => R) = {
    val start = System.currentTimeMillis
    block
    println("Block (" + blockName + ") took " + (System.currentTimeMillis - start) + "ms.")
  }

  /*
  def copy(a: Array[Array[Int]]) : Array[Array[Int]] = {
	  for(inner <- a) yield {
		  for (elem <- inner) yield {
			  elem
		  }
	  }
  }
  */

}