// P16 (**) Drop every Nth element from a list.
// Example:
// scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
import scala.annotation.tailrec

object P16 {

  def drop[A](n: Int, ls: List[A]): List[A] = {
    @tailrec
    def dropHelper[A](c: Int, ls: List[A], accum: List[A]): List[A] = (c, ls) match {
      case (_, Nil) => accum.reverse
      case (1, x :: xs) => dropHelper(n, xs, accum)
      case (_, x :: xs) => dropHelper(c - 1, xs, x :: accum)
    }
    dropHelper(n, ls, List())
  }

  def main(args: Array[String]) {
    val a = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(a)
    println(drop(3, a))
  }
}