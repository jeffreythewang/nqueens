package nqueens

import akka.actor._

object ChessFunctions {

  // Places a queen in the zeroth row of a board and filters out offending spaces
  def placeQueen(column: Int, currentBoard: List[List[Int]]) : List[List[Int]] = {
    currentBoard.zipWithIndex.map { case (row, rowIdx) =>
      row.filter(colIdx => !columnFilter(column, colIdx) && !lowerSlopeFilter(0, column, rowIdx, colIdx))
    }
  }

  def columnFilter(occupiedColumn: Int, column: Int ): Boolean = occupiedColumn == column

  def lowerSlopeFilter(occupiedRow: Int, occupiedColumn: Int, row: Int, column: Int): Boolean = {
    row - occupiedRow == Math.abs(column - occupiedColumn)
  }

}

class Distributor(numSolvers: Int, startTime: Long) extends Actor {

  val solvers = Vector.fill(numSolvers)(context.actorOf(Props(new Solver(self))))

  def receive = {
    case TryRow(board) => distribute(board)
    case Solution(solutionList) =>
      if (solutionList.nonEmpty) {
        val now = System.currentTimeMillis()
        println(now - startTime)
        println(solutionList)
      }
  }

  def distribute(board: List[List[Int]]): Unit =
    board.head match {
      case head :: tail =>
        solvers(board.head.length % numSolvers) ! TryRow(board)
        val boardToTry = List(board.head.tail) ++ board.tail
        distribute(boardToTry)
      case List() =>
    }
}

case class TryRow(board: List[List[Int]])
case class Solution(solutionList: List[Int])
case object FailedSolution

class Solver(distributor: ActorRef) extends Actor {

  import ChessFunctions._

  def receive = {
    case TryRow(board) =>
      val solution = tryRow(board)
      if (solution != Nil) {
        distributor ! Solution(solution)
      } else {
        distributor ! FailedSolution
      }
  }

  def tryRow(board: List[List[Int]]) : List[Int] = board match {
    case rows if rows.length > 1 =>
      if (rows.contains(List())) {
        // solution failed
        Nil
      } else {
        val rowToTry = tryRow(placeQueen(board.head.head, board).tail)
        if (rowToTry != Nil) {
          List(board.head.head) ++ rowToTry
        } else {
          val newBoard = List(board.head.tail) ++ board.tail
          tryRow(newBoard)
        }
      }
    case rows if rows.length == 1 =>
      if (rows.contains(List())) {
        // solution failed
        Nil
      } else {
        List(rows.head.head)
      }
  }
}

object Main {

  import ChessFunctions._

  def main(args: Array[String]) {

    val system = ActorSystem("main")

    val n = 28

    val sampleBoard = List.fill(n)((0 until n).toList)

    val numSolvers = 5

    val now = System.currentTimeMillis()

    val distributor = system.actorOf(Props(new Distributor(numSolvers, now)))

    println("begin run")
    distributor ! TryRow(sampleBoard)
  }
}
