package com.sciolizer.astar.examples.sokoban

import com.sciolizer.astar.AStar
import com.sciolizer.astar.AStar.{Admissable, HeuristicGuarantee, Domain}
import collection.mutable
import java.util.Comparator

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/10/13
 * Time: 7:12 PM
 */
object Sokoban {

  def solve(board: Board): List[Direction] = {
    val domain = new SokobanDomain(makeDistanceMap(board.squares, board.goals))
    AStar.search(board, domain)._1
  }

  def makeDistanceMap(board: Vector[Vector[Square]], goals: Set[Point]): Map[Point, Int] = {
    object DistanceDomain extends Domain[Point, Direction, Int] {
      def children(s: Point): Map[Direction, (Point, Int)] =
        (for (d <- Direction.all; if d.toPoint(s).of(board) == Some(Blank)) yield (d, (d.toPoint(s), 1))).toMap
      def heuristicGuarantee: HeuristicGuarantee = Admissable()
      def heuristicFunction(s: Point): Double = goals.map(_.taxicab(s)).min
      def isGoal(s: Point): Boolean = goals.contains(s)
      def zero: Int = 0
      def add(m1: Int, m2: Int): Int = m1 + m2
      lazy val c = new Comparator[Int] {
        def compare(o1: Int, o2: Int): Int = math.signum(o2 - o1)
      }
      def comparator: Comparator[Int] = c
    }

    var ret: Map[Point, Int] = Map.empty
    for (r <- board.size) {
      for (c <- board.size) {
        val point: Point = Point(r, c)
        ret = ret.updated(point, if (board(r)(c) == Wall) {
          Int.MaxValue
        } else {
          AStar.search[Point, Direction, Int](point, DistanceDomain)._2
        })
      }
    }
    ret
  }
}

class SokobanDomain(grid: Vector[Vector[Square]], goals: Set[Point], distanceMap: Map[Point, Int])
  extends Domain[Board, Direction, Distance] {

  case class Changeable(player: Point, boxes: Set[Point])

  val memoized: mutable.Map[Changeable, Distance] = mutable.Map.empty

  def children(s: Board): Map[Direction, (Board, Distance)] =
    (for (d <- Direction.all; Some(b) <- d.toBoard(s)) yield ((d, (b,  Distance(if (d.movesBox(s)) 1 else 0, 0))))).toMap
  def heuristicGuarantee: HeuristicGuarantee = Admissable()
  def isGoal(s: Board): Boolean = s.goals == s.boxes
  def zero: Distance = Distance(0, 0)
  def add(m1: Distance, m2: Distance): Distance = Distance(m1.boxMoves + m2.boxMoves, m1.playerMoves + m2.playerMoves)

  private lazy val c = new Comparator[Distance] {
    def compare(o1: Distance, o2: Distance): Int = {
      if (o1.boxMoves != o2.boxMoves) {
        math.signum(o2.boxMoves - o1.boxMoves)
      } else {
        math.signum(o2.playerMoves - o1.playerMoves)
      }
    }
  }

  def comparator: Comparator[Distance] = c

  def heuristicFunction(s: Board): Distance = {
    if (s.boxes.isEmpty) {
      Distance(0, distanceMap(s.player))
    } else {
      memoized.get(Changeable(s.player, s.boxes)) match {
        case Some(h) => h
        case None =>
          if (s.boxes.size == 1) {
            val box: Point = s.boxes.head
            Distance(distanceMap(box), box.taxicab(s.player) - 1)
          } else {
            def distanceOf(boxes: Set[Point]): Distance = {
              val subBoard: Board = s.copy(boxes = boxes)
              val key: SokobanDomain.this.type#Changeable = Changeable(s.player, boxes)
              memoized.get(key) match {
                case Some(h) => h
                case None =>
                  val ret = AStar.search(subBoard, this)._2
                  memoized(key) = ret
                  ret
              }
            }
            val (left, right) = s.boxes.splitAt(s.boxes.size / 2)
            val dl = distanceOf(left)
            val dr = distanceOf(right)
            Distance(dl.boxMoves + dr.boxMoves, dl.playerMoves max dr.playerMoves)
          }
      }
    }
  }

}

abstract class Direction {
  def toPoint(p: Point): Point
  def toBoard(board: Board): Option[Board] = {
    val next = toPoint(board.player)
    if (board.get(next) != Some(Blank)) {
      None
    } else {
      if (!board.boxes.contains(next)) {
        Some(board.copy(player = next))
      } else {
        val nextNext = toPoint(next)
        if (board.get(nextNext) != Some(Blank) || board.boxes.contains(nextNext)) {
          None
        } else {
          Some(board.copy(goals = board.goals - next + nextNext, player = next))
        }
      }
    }
  }
  def movesBox(board: Board): Boolean = {
    val next: Point = toPoint(board.player)
    board.boxes.contains(next) && board.get(toPoint(next)) == Some(Blank)
  }
}
object Direction {
  def all: List[Direction] = List(Up, Down, Left, Right)
}
object Up extends Direction {
  def toPoint(p: Point): Point = p.copy(row = p.row - 1)
}
object Down extends Direction {
  def toPoint(p: Point): Point = p.copy(row = p.row + 1)
}
object Left extends Direction {
  def toPoint(p: Point): Point = p.copy(column = p.column - 1)
}
object Right extends Direction {
  def toPoint(p: Point): Point = p.copy(column = p.column + 1)
}
case class Distance(boxMoves: Int, playerMoves: Int)

case class Point(row: Int, column: Int) {
  def of(grid: Vector[Vector[Square]]): Option[Square] = for (r <- grid.lift(row); spot <- r.lift(column)) yield spot
  def taxicab(that: Point): Int = math.abs(row - that.row) + math.abs(column - that.column)
}
case class Board(squares: Vector[Vector[Square]], goals: Set[Point], boxes: Set[Point], player: Point) {
  def get(p: Point): Option[Square] = p.of(squares)
}
abstract class Square
object Wall extends Square
object Blank extends Square