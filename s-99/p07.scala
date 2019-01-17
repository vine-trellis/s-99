// P07 (**) Flatten a nested list structure.
// Example:
// scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
// res0: List[Any] = List(1, 1, 2, 3, 5, 8)

object P07 {

  def flattenRecursive(ls: List[Any]): List[Any] = 
    { 
      if (ls.isEmpty) {
        List()
      } else {
        ls.head match {
          case a: List[Any] => flattenRecursive(a) ::: flattenRecursive(ls.tail)
          case b if !ls.tail.isEmpty => b :: flattenRecursive(ls.tail)
          case c if ls.tail.isEmpty => List(c)
        }
      }
    }


  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  def time[T](block: => T): T = {
    val start = System.nanoTime
    val res = block
    val totalTime = System.nanoTime - start
    println("Elapsed time: %1d ns".format(totalTime))
    res
  }

  def main(args: Array[String]) {
      val a = List(List(1, 1), 2, List(3, List(5, 8)))
      time(flatten(a))
      time(flattenRecursive(a))
      time(flattenRecursive2(a))
    //   println(flatten(a))
    //   println(flattenRecursive(a))
  }
}