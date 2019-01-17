// 696. Count Binary Substrings
// Easy
// 503
// 88


// Give a string s, count the number of non-empty (contiguous) substrings that have the same number of 0's and 1's, 
// and all the 0's and all the 1's in these substrings are grouped consecutively.

// Substrings that occur multiple times are counted the number of times they occur.

// Example 1:
// Input: "00110011"
// Output: 6
// Explanation: There are 6 substrings that have equal number of consecutive 1's and 0's: "0011", "01", "1100", "10", "0011", and "01".

// Notice that some of these substrings repeat and are counted the number of times they occur.

// Also, "00110011" is not a valid substring because all the 0's (and 1's) are not grouped together.
// Example 2:
// Input: "10101"
// Output: 4
// Explanation: There are 4 substrings: "10", "01", "10", "01" that have equal number of consecutive 1's and 0's.
// Note:

// s.length will be between 1 and 50,000.
// s will only consist of '0' or '1' characters.

object Solution {
  def countBinarySubstrings(s: String): Int = {
    def countHelper(ss: List[Char], ct: Int, nt: Int, tracker: Char, ac: Int): Int = {
      (ss, tracker, ct >= nt) match {
        case (Nil, _, _) => ac
        case (x :: xs, '1', true) if (x == '1') => countHelper(xs, ct, nt + 1, '1', ac + 1)
        case (x :: xs, '0', true) if (x == '0') => countHelper(xs, ct, nt + 1, '0', ac + 1)
        case (x :: xs, '1', false) if (x == '1') => countHelper(xs, ct, nt + 1, '1', ac + 1)
        case (x :: xs, '0', false) if (x == '0') => countHelper(xs, ct, nt + 1, '0', ac + 1)
        case (x :: xs, '1', _) if (x == '0') => countHelper(xs, nt, 0, '0', ac)
        case (x :: xs, '0', _) if (x == '1') => countHelper(xs, nt, 0, '1', ac)
      }
    }
    countHelper(s.toList, 0, 0, s.head, 0)
  }
  def main(args: Array[String]): Unit = {
    println(countBinarySubstrings("00110011"))
    println(countBinarySubstrings("10101"))
  }
}