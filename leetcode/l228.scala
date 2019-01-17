// 228. Summary Ranges
// Medium
// Given a sorted integer array without duplicates, return the summary of its ranges.
// 
// Example 1:
// Input:  [0,1,2,4,5,7]
// Output: ["0->2","4->5","7"]
// Explanation: 0,1,2 form a continuous range; 4,5 form a continuous range.
// 
// Example 2:
// Input:  [0,2,3,4,6,8,9]
// Output: ["0","2->4","6","8->9"]
// Explanation: 2,3,4 form a continuous range; 8,9 form a continuous range.
import scala.annotation.tailrec
object L228 extends App {
  val a = Array(1,3)
  println(a.deep)
  println(summaryRanges(a))

  def summaryRanges(nums: Array[Int]): List[String] = {
    @tailrec
    def summaryRangesHelper(start: Int, prev: Int, ns: List[Int],  ac: List[String]): List[String] = {
      (start == prev, ns.head - prev, ns) match {
        case (_,    1, x :: Nil)  => ((start.toString + "->" + x.toString) :: ac).reverse
        case (true, _, x :: Nil)  => (x.toString :: (start.toString) :: ac).reverse
        case (_,    _, x :: Nil)  => (x.toString :: (start.toString + "->" + prev.toString) :: ac).reverse
        case (_,    1, x :: xs)   => summaryRangesHelper(start, x, xs, ac)
        case (true, _, x :: xs)   => summaryRangesHelper(x, x, xs, (start.toString) :: ac)
        case (_,    _, x :: xs)   => summaryRangesHelper(x, x, xs, (start.toString + "->" + prev.toString) :: ac)
      }
    }
    nums.toList match {
      case Nil => Nil
      case x :: Nil => x.toString :: Nil
      case x => summaryRangesHelper(x.head, x.head, x.tail, Nil)
    }
  }
}