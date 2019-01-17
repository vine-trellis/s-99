// P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
// In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
// Example:

// scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
// res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...

import scala.annotation.tailrec

object P26 {
  def combinations[A](n: Int, ls: List[A]): List[List[A]] = {
    def comboHelper[A](c: Int, cur: List[A], accum: List[A]): List[List[A]] = {
      println(s"c: $c")
      println(s"cur: $cur")
      println(s"accum: $accum")
      (c, cur) match {
        case (_, Nil) => List(Nil)
        case (1, _) => {
          val a = cur.map(v => (v :: accum).reverse)
          println(s"a: $a")
          a
        }
        case (_, x :: xs) => comboHelper(c - 1, xs, x :: accum)
      }
    }
    (n, ls) match {
      case (_, _) if n == ls.length => List(ls)
      case (_, x :: xs) => comboHelper(n, ls, List()) ::: combinations(n, xs)
    }
  }

  def main(args: Array[String]) {
    val a = List('a, 'b, 'c, 'd, 'e, 'f)
    println(a)
    println(combinations(3, a))
  }
}