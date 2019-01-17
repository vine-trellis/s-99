// According to the Wikipedia's article: 
// "The Game of Life, also known simply as Life, is a 
// cellular automaton devised by the British mathematician 
// John Horton Conway in 1970."
// 
// Given a board with m by n cells, each cell has an 
// initial state live (1) or dead (0). Each cell interacts 
// with its eight neighbors (horizontal, vertical, diagonal) 
// using the following four rules (taken from the above Wikipedia article):
// 
// Any live cell with fewer than two live neighbors dies, as if caused by under-population.
// Any live cell with two or three live neighbors lives on to the next generation.
// Any live cell with more than three live neighbors dies, as if by over-population..
// Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
// Write a function to compute the next state (after one update) of the board given its current state. 
// The next state is created by applying the above rules simultaneously 
// to every cell in the current state, where births and deaths occur simultaneously.
// 
// Example:
// 
// Input: 
// [
//   [0,1,0],
//   [0,0,1],
//   [1,1,1],
//   [0,0,0]
// ]
// Output: 
// [
//   [0,0,0],
//   [1,0,1],
//   [0,1,1],
//   [0,1,0]
// ]
// Follow up:
// 
// Could you solve it in-place? Remember that the board needs to be updated at the same time: 
// You cannot update some cells first and then use their updated values to update other cells.
// In this question, we represent the board using a 2D array. 
// In principle, the board is infinite, which would cause problems when the active area 
// encroaches the border of the array. How would you address these problems?

object Solution {
  def gameOfLife(board: Array[Array[Int]]): Unit = {
    def countLiveNeighboursTR(x: Int, y: Int, a: Int, b: Int, accum: Int): Int = {
      (a, b) match {
        case (0, 0) => countLiveNeighboursTR(x, y, a, b, accum)
        case (1, 1) => accum + (board(x + a)(y + b) >> 1)
        case (1, _) => countLiveNeighboursTR(x, y, a, b + 1, accum + (board(x + a)(y + b) >> 1))
        case (_, _) => countLiveNeighboursTR(x, y, a + 1, b, accum + (board(x + a)(y + b) >> 1))
      }
    }
    def countLiveNeighbours(x :Int, y :Int): Int = {
      var accum = 0
      for (a <- x -1 to x + 1;b <- y -1 to y + 1) {
        if (
              a >= 0 && a < board.length &&
              b >= 0 && b < board(0).length &&
              !(a == x && b == y) && 
              ((board(a)(b) & 2) == 2)
        ) accum = accum + 1 
      }
      accum
    }      
    for (i <- 0 until board.length) board(i).transform(_ << 1)
    for (i <- 0 until board.length) {
      for (j <- 0 until board(i).length) {
        board(i)(j) = (board(i)(j) >> 1, countLiveNeighbours(i,j)) match {
          case (0, 3) => board(i)(j) | 1
          case (1, 2) => board(i)(j) | 1
          case (1, 3) => board(i)(j) | 1
          case (_, _) => board(i)(j) & 2
        }
      }
    }
    for (i <- 0 until board.length) board(i).transform(_ & 1)
  }

  def main(args: Array[String]): Unit = {
    var a = Array(
      Array(0,1,0),
      Array(0,0,1),
      Array(1,1,1),
      Array(0,0,0)
    )
    gameOfLife(a)
  }
}