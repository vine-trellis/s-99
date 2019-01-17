// P05 (*) Reverse a list.
// Example:
// scala> reverse(List(1, 1, 2, 3, 5, 8))
// res0: List[Int] = List(8, 5, 3, 2, 1, 1)

object P05 {
  def reverse[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case h :: t => reverse(t) ::: List(h)
  }
  // Pure functional
  def reverseFunctional[A](ls: List[A]): List[A] =
    ls.foldLeft(List[A]()) { (r, h) => h :: r }

  def main(args: Array[String]) {
    val a = List(1, 1, 2, 3, 5, 8)
    println(a)
    println(reverse(List(1, 1, 2, 3, 5, 8)))
    println(reverseFunctional(List(1, 1, 2, 3, 5, 8)))
  }

}