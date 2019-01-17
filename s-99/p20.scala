// P20 (*) Remove the Kth element from a list.
// Return the list and the removed element in a Tuple. Elements are numbered from 0.
// Example:

// scala> removeAt(1, List('a, 'b, 'c, 'd))
// res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)

import scala.annotation.tailrec

object P20 {
  def removeAt[A](index: Int, ls: List[A]): (List[A], Option[A]) = {
    // val a = ls.zipWithIndex
    // (a.filterNot(_._2 == index).map(_._1), a.)
    @tailrec
    def removeHelper[A](i: Int, c: List[A], r: List[A]): (List[A], Option[A]) = {
      (i, c) match {
        case (_, _) if i < 0 => (c, None)
        case (_, Nil) => (c, None)
        case (0, x :: xs) => (r.reverse ::: xs, Some(x)) 
        case (_, x :: xs) => removeHelper(i - 1, xs, x :: r)
      }
    }
    removeHelper(index, ls, Nil)
  }

  def main(args: Array[String]) {
    val a = List('a, 'b, 'c, 'd)
    println(a)
    println(removeAt(-1, a))
  }
}