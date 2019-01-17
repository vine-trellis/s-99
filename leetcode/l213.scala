// You are a professional robber planning to rob houses along a street. 
// Each house has a certain amount of money stashed. All houses at this place are arranged in a circle. 
// That means the first house is the neighbor of the last one. 
// Meanwhile, adjacent houses have security system connected and it will automatically contact the police 
// if two adjacent houses were broken into on the same night.

// Given a list of non-negative integers representing the amount of money of each house, 
// determine the maximum amount of money you can rob tonight without alerting the police.

// Example 1:

// Input: [2,3,2]
// Output: 3
// Explanation: You cannot rob house 1 (money = 2) and then rob house 3 (money = 2),
//              because they are adjacent houses.
// Example 2:

// Input: [1,2,3,1]
// Output: 4
// Explanation: Rob house 1 (money = 1) and then rob house 3 (money = 3).
//              Total amount you can rob = 1 + 3 = 4.
object Solution {
  def rob(nums: Array[Int]): Int = {
    def robHelper(n: Array[Option[Int]]): Int = {
      println(s"n: ${n.deep}")
      var m = new Array[Int](n.size)
      for (i <- 0 until n.size) {
        if (!n(i).isEmpty && n(i).getOrElse(0) > -1) {
          m(i) = {
            n(i).get
            - n((i - 1 + n.size) % n.size).getOrElse(0)
            - n((i + 1)%n.size).getOrElse(0)
          }
        }
      }
      println(s"m: ${m.deep}")
      m.indexOf(m.max)
    }
    var numsSome: Array[Option[Int]] = nums.map(Some(_))
    var dummyIndex = 0
    for (i <- 0 until nums.size - 2) {
      dummyIndex = robHelper(numsSome)
      println(s"numsSome: ${numsSome.deep}, i: $i, maxindex: $dummyIndex")
      numsSome(dummyIndex) = None
      numsSome((dummyIndex - 1 + nums.size) % nums.size) = None
      numsSome((dummyIndex + 1) % nums.size) = None

      nums((dummyIndex - 1 + nums.size) % nums.size) = 0
      nums((dummyIndex + 1) % nums.size) = 0
    }
    println(s"before sum ${nums.deep}")
    // -numsSome.map(_.getOrElse(0)).sum
    nums.sum
  }
  def main(args: Array[String]): Unit = {
    val a = Array(2,3,2,1,2,3,1)
    // println(a.deep)
    println(rob(a))
  }
}