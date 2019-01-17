// P21 (*) Insert an element at a given position into a list.
// Example:
// scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
// res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)

import scala.annotation.tailrec

object P20 {
  def insertAt[A](elem: A, index: Int, ls: List[A]): List[A] = {
    def insertHelper[A](e: A, n: Int, c: List[A], r: List[A]): List[A] = (n, c) match {
      case (_, Nil) => (e :: r).reverse
      case (0, _) => r.reverse ::: e :: c
      case (_, x :: xs) => insertHelper(e, n - 1, xs, x :: r)
    }
    insertHelper(elem, index, ls, Nil)
  }

  def main(args: Array[String]) {
    val a = List('a, 'b, 'c, 'd)
    println(a)
    println(insertAt('new, 1, a))
  }
}