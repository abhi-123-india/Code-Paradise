package backtracking

import scala.collection.mutable
import scala.collection.mutable._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

/**
 * Created by abhijeet on 21-02-2015.
 */
object SudokuSolver {
  // Sample input
  // This is a 9x9 matrix
  val grid = Array[Array[Int]](
    Array[Int](3, 0, 6, 5, 0, 8, 4, 0, 0),
    Array[Int](0, 2, 0, 0, 0, 0, 0, 0, 0),
    Array[Int](5, 8, 7, 0, 0, 0, 0, 3, 1),
    Array[Int](0, 0, 3, 0, 1, 0, 0, 8, 0),
    Array[Int](9, 0, 0, 8, 6, 3, 0, 0, 5),
    Array[Int](0, 5, 0, 0, 9, 0, 6, 0, 0),
    Array[Int](1, 3, 0, 0, 0, 0, 2, 5, 0),
    Array[Int](0, 0, 0, 0, 0, 0, 0, 7, 4),
    Array[Int](4, 0, 5, 2, 0, 6, 3, 0, 0))

  // Find next unfilled position
  def getNextPos(grid: MutableList[MutableList[Int]]): (Boolean, (Int, Int)) = {
    grid.zipWithIndex.filter(t => (t._1.filter(_ != 0)).size < t._1.length) match {
      case fg if (fg.isEmpty) => (true, (-1, -1))
      case fg => (false, fg.head match {
        case (lst, pos) => (fg.head._2, (fg.head._1).zipWithIndex.filter(_._1 == 0).head._2)
        case _ => throw new IllegalArgumentException
      })
    }
  }

  def getCol(grid: MutableList[mutable.MutableList[Int]], col: Int): List[Int] = {
    (for (i <- 0 until grid.length) yield grid(i)(col)).filter(_ != 0).toList
  }

  def printGrid(grid: MutableList[mutable.MutableList[Int]]): String = {
    grid.map(_.mkString(",")).mkString("\n")
  }

  def recStackPop(grid: MutableList[mutable.MutableList[Int]], stack:Stack[(Int,Int,List[Int],List[Int])]):(Boolean,MutableList[mutable.MutableList[Int]], Stack[(Int,Int,List[Int],List[Int])]) = {
    stack match {
      case stk if (stk.isEmpty) => {
        println("Stack is empty, this is a unsolvable problem")
        (false,grid,stk)

      }
      case stk => stk.pop() match {
        case (row,col, used,fut) if(fut.isEmpty) => {
          println(s"All options are exhausted, for ${row}${col}, used ${used.mkString(""",""")}")
          grid(row)(col) = 0
          recStackPop(grid,stk)
        }
        case (row,col,used,fut) => {
          // we need to be careful here
          println(s"row:${row}, col:${col} for changing the value from ${grid(row)(col)} to ${fut.head}")
          grid(row)(col) = fut.head
          stk.push((row,col,used.++(List(fut.head)),fut.tail.toList))
          (true,grid,stk)
        }
      }
    }
  }

  def solveSudoku(grid: MutableList[mutable.MutableList[Int]], stack:Stack[(Int,Int,List[Int],List[Int])]=Stack[(Int,Int,List[Int],List[Int])]()):Boolean = {
    // ensure that grid represents square matrix in mutable form
    assert((grid.foldLeft(0)(_ + _.length))/(grid.length) == grid.length)
    getNextPos(grid) match {
      case (true,(row,col)) => {
        println(s"Solved:\n${printGrid(grid)}");true
      }
      case (false,(row,col)) => {
        val posRepl = (1 until 10).diff(getCol(grid,col)).diff(grid.apply(row).filter(_!=0))
        posRepl match {
          case prl if(prl.isEmpty) => {
            // we got here since there is an overlap
            // we need to backtrack
            // pops from stk and change the option
            grid(row)(col) = 0
            println(s"Unwinding from row:${row},col:${col}")
            recStackPop(grid,stack) match {
              case (false,gr,st) => println("unsolvable");false
              case (true,gr,st) => solveSudoku(gr,st)
            }
          }
          case sl => {
            println(s"for row:${row} col:${col} changing the value from ${grid(row)(col)} to ${sl.head}")
            grid(row)(col) = sl.head
            stack.push((row,col,List(sl.head),sl.tail.toList))
            println(s"new set used is ${stack.apply(0)._3.mkString(""",""")}, future:${(stack.apply(0)._4.mkString(""","""))}")
            solveSudoku(grid,stack)
          }
        }
      }
    }
  }
  def arrayToMutableList[T](arr:Array[T]):mutable.MutableList[T] = mutable.MutableList() ++= arr
  def getMutableGrid[T](grid:Array[Array[T]]) = ((mutable.MutableList[mutable.MutableList[T]])()/:(grid.toList))(_ += arrayToMutableList[T](_))
  def main(args:Array[String]):Unit = {
    val lst = getMutableGrid[Int](grid)
    solveSudoku(lst)
    println("finished")
  }
}
