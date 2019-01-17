// P04 (*) Find the number of elements of a list.
// Example:
// scala> length(List(1, 1, 2, 3, 5, 8))
// res0: Int = 6

object P04 {
  def length(ls: List[_]): Int = ls match {
    case Nil => 0
    case _ :: tail => 1 + length(tail)
  }

  def main(args: Array[String]) {
    val a = List(1, 1, 2, 3, 5, 8)
    println(a)
    println(length(List(1, 1, 2, 3, 5, 8)))
  }

}