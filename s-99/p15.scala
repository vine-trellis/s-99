// P15 (**) Duplicate the elements of a list a given number of times.
// Example:
// scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
// res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
import scala.annotation.tailrec

object P15 {
  def duplicateN[A](n: Int, ls: List[A]): List[A] = {
    @tailrec
    def dupHelper[A](n: Int, element: A, accum: List[A]): List[A] = n match {
      case 0  => accum
      case x => dupHelper(x - 1, element, element :: accum)
    }
    ls match {
      case Nil => ls
      case x :: xs => dupHelper(n, x, List()) ::: duplicateN(n, xs)
    } 
  }

  def main(args: Array[String]) {
    val a = List('a, 'b, 'c, 'c, 'd)
    println(a)
    println(duplicateN(3, a))
  }
}