// P08 (**) Eliminate consecutive duplicates of list elements.
// If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
// Example:

// scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

object P08 {
  // def compress[A](ls: List[A]): List[A] = {
  //   ls.head.foldLeft(
  //     (x,y) => if (x == y) {
  //       List(x)
  //   } else {
  //     List(x, y)
  //   }
  //   )
  // }

  // Functional.
  def compressFunctional[A](ls: List[A]): List[A] =
    ls.foldRight(List[A]()) { (h, r) =>
      if (r.isEmpty || r.head != h) h :: r
      else r
    }

  def main(args: Array[String]) {
    val a = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(a)
    println(compressFunctional(a))
  }
}