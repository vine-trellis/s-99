// 240. Search a 2D Matrix II
// Medium
// 1052
// 33


// Write an efficient algorithm that searches for a value in an m x n matrix. This matrix has the following properties:

// Integers in each row are sorted in ascending from left to right.
// Integers in each column are sorted in ascending from top to bottom.
// Example:

// Consider the following matrix:

// [
//   [1,   4,  7, 11, 15],
//   [2,   5,  8, 12, 19],
//   [3,   6,  9, 16, 22],
//   [10, 13, 14, 17, 24],
//   [18, 21, 23, 26, 30]
// ]
// Given target = 5, return true.

// Given target = 20, return false.

import scala.annotation.tailrec
object L240 extends App {
  val a =  Array(
  Array(1,   4,  7, 11, 15),
  Array(2,   5,  8, 12, 19),
  Array(3,   6,  9, 16, 22),
  Array(10, 13, 14, 17, 24),
  Array(18, 21, 23, 26, 30)
  )

  println(a.deep)
  println(searchMatrix(a, 5))

  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    def searchMatrixHelper(i: Int, j: Int): Boolean = (matrix(i)(j), target) match {
      case (a, b) if (a == b) => true
      case (a, b) if (a > b) => searchMatrixHelper(i/2, j)
      case (a, b) if (a < b) => searchMatrixHelper(i + i/2, j)
    }
    searchMatrixHelper(matrix.length/2, matrix(0).length/2)
  }
}