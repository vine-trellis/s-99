// P09 (**) Pack consecutive duplicates of list elements into sublists.
// If a list contains repeated elements they should be placed in separate sublists.
// Example:

// scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), 
//    List('a, 'a), List('d), List('e, 'e, 'e, 'e))

object P09 {
  def pack[A](ls: List[A]): List[List[A]] = {
    ls.foldLeft(
      (x,y) => if (x == y) {
        
      }
    )
  }
  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  def main(args: Array[String]) {
    val a = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(a)
    println(pack(a))
  }
}