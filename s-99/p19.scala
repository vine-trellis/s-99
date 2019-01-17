// P19 (**) Rotate a list N places to the left.
// Examples:
// scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

// scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

import scala.annotation.tailrec

object P19 {
  @tailrec
  def rotate[A](n: Int, ls: List[A]): List[A] = {
    val nBound: Int = if (n < 0) n + ls.length else n
    (nBound, ls) match {
      case (_, Nil) => Nil
      case (0, _) => ls
      case (_, x :: xs) => rotate(n - 1, xs :+ x)
    }
  }

  def main(args: Array[String]) {
    val a = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(a)
    println(rotate(3, a))
    println(rotate(-2, a))
  }
}