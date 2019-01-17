// 352. Data Stream as Disjoint Intervals
// Hard
// 129
// 45

// Given a data stream input of non-negative integers 
// a1, a2, ..., an, ..., summarize the numbers seen so far as a list of disjoint intervals.

// For example, suppose the integers from the data 
// stream are 1, 3, 7, 2, 6, ..., then the summary will be:

// [1, 1]
// [1, 1], [3, 3]
// [1, 1], [3, 3], [7, 7]
// [1, 3], [7, 7]
// [1, 3], [6, 7]
// Follow up:
// What if there are lots of merges and the number of 
// disjoint intervals are small compared to the data stream's size?


/**
 * Definition for an interval.
 * class Interval(var _start: Int = 0, var _end: Int = 0) {
 *   var start: Int = _start
 *   var end: Int = _end
 * }
 */
class Interval(var _start: Int = 0, var _end: Int = 0) {
   var start: Int = _start
   var end: Int = _end
}

class SummaryRanges() {
  /** Initialize your data structure here. */
  var ls: List[Interval] = List()
  def addNum(`val`: Int) {
    if (ls == Nil) {
      ls = new Interval(`val`, `val`) :: ls
    }  
  }
  def getIntervals(): List[Interval] = {
    ls
  }
}

object l352 {
  def main(args: Array[String]): Unit = {
    var obj = new SummaryRanges()
    obj.addNum(1)
    var param_2 = obj.getIntervals()
    // println(s"${param_2.head.start}, ${param_2.head.end}")
    prIntervals(param_2)
  }
  def prIntervals(is: List[Interval]){
    is.foreach(x =>
      println(s"${x.start}, ${x.end}")
    )
  }
}
/**
 * Your SummaryRanges object will be instantiated and called as such:
 * var obj = new SummaryRanges()
 * obj.addNum(`val`)
 * var param_2 = obj.getIntervals()
 */