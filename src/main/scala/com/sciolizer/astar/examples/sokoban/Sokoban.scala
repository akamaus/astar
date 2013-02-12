package com.sciolizer.astar.examples.sokoban

import com.sciolizer.astar.AStar
import com.sciolizer.astar.AStar.{IntMeasure, SearchDomain, Admissable, HeuristicGuarantee}
import collection.mutable
import java.util.Comparator

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/10/13
 * Time: 7:12 PM
 */
object Sokoban {

  def solve(board: Board): Option[List[Direction]] = {
    val domain = new SokobanDomain(board.squares, board.goals, makeDistanceMap(board.squares, board.goals))
    for (s <- AStar.search(board, domain)) yield s._1
  }

  def makeDistanceMap(board: Vector[Vector[Square]], goals: Set[Point]): Map[Point, Int] = {
    object DistanceDomain extends IntMeasure with SearchDomain[Point, Direction, Int] {
      def children(s: Point): Map[Direction, (Point, Int)] =
        (for (d <- Direction.all; if d.toPoint(s).of(board) == Some(Blank)) yield (d, (d.toPoint(s), 1))).toMap
      def heuristicGuarantee: HeuristicGuarantee = Admissable()
      def heuristicFunction(s: Point): Int = goals.map(_.taxicab(s)).min
      def isGoal(s: Point): Boolean = goals.contains(s)
    }

    var ret: Map[Point, Int] = Map.empty
    for (r <- 0 until board.size) {
      for (c <- 0 until board(r).size) {
        val point: Point = Point(r, c)
        ret = ret.updated(point, if (board(r)(c) == Wall) {
          Int.MaxValue
        } else {
          AStar.search[Point, Direction, Int](point, DistanceDomain) match {
            case None => Int.MaxValue
            case Some((_, d)) => d
          }
        })
      }
    }
    ret
  }
}

class SokobanDomain(grid: Vector[Vector[Square]], goals: Set[Point], distanceMap: Map[Point, Int])
  extends SearchDomain[Board, Direction, Distance] {

  case class Changeable(player: Point, boxes: Set[Point])

  val memoized: mutable.Map[Changeable, Distance] = mutable.Map.empty

  def children(s: Board): Map[Direction, (Board, Distance)] =
    (for (d <- Direction.all; if d.toBoard(s).isDefined) yield ((d, (d.toBoard(s).get,  Finite(if (d.movesBox(s)) 1 else 0, 0))))).toMap
  def heuristicGuarantee: HeuristicGuarantee = Admissable()
  def isGoal(s: Board): Boolean = s.boxes.subsetOf(s.goals)
  def zero: Distance = Finite(0, 0)
  def add(m1: Distance, m2: Distance): Distance = {
    (m1, m2) match {
      case (Infinite(), _) => Infinite()
      case (_, Infinite()) => Infinite()
      case (Finite(bm1, pm1), Finite(bm2, pm2)) => {
        val boxMoves: Int = bm1 + bm2
        Finite(boxMoves, pm1 max pm2 max boxMoves)
      }
    }
  }

  private lazy val c = new Comparator[Distance] {
    def compare(o1: Distance, o2: Distance): Int = {
      (o1, o2) match {
        case (Infinite(), Infinite()) => 0
        case (Infinite(), Finite(_, _)) => 1
        case (Finite(_, _), Infinite()) => -1
        case (Finite(bm1, pm1), Finite(bm2, pm2)) =>
          if (bm1 != bm2) {
            math.signum(bm1 - bm2)
          } else {
            math.signum(pm1 - pm2)
          }
      }
    }
  }

  def comparator: Comparator[Distance] = c

  def heuristicFunction(s: Board): Distance = {
    val ret: Distance = if (s.boxes.isEmpty) {
      Finite(0, distanceMap(s.player))
    } else if (s.boxes.subsetOf(s.goals)) {
      Finite(0, 0)
    } else {
      print("memoized size: " + memoized.size)
      memoized.get(Changeable(s.player, s.boxes)) match {
        case Some(h) =>
          println("; direct hit")
          h
        case None =>
          print("; miss; ")
          if (s.boxes.size == 1) {
            val box: Point = s.boxes.head
            if (box == s.player) {
              throw new IllegalStateException("impossible board: " + s.player + " and " + s.boxes)
            }
            val tc: Int = box.taxicab(s.player)
            if (tc == 0) {
              throw new IllegalStateException("taxicab is zero: " + s.player + " and " + s.boxes)
            }
            val boxMoves: Int = distanceMap(box)
            println("no store")
            Finite(boxMoves, tc + boxMoves - 1)
          } else {
            def distanceOf(boxes: Set[Point]): Distance = {
              val subBoard: Board = s.copy(boxes = boxes)
              val key: Changeable = Changeable(s.player, boxes)
              memoized.get(key) match {
                case Some(h) =>
                  print("inner hit; ")
                  h
                case None =>
                  print("inner miss; ")
                  val ret = AStar.search(subBoard, this) match {
                    case None =>
                      memoized(key) = Infinite()
                      Infinite()
                    case Some((steps, d)) =>
                      var curKey = key
                      memoized(key) = d
                      var Finite(boxMoves, playerMoves) = d
                      var curBoard = s.copy(boxes = curKey.boxes, player = curKey.player)
                      for (direction <- steps) {
                        if (direction.movesBox(curBoard)) {
                          boxMoves -= 1
                        }
                        playerMoves -= 1
                        curBoard = direction.toBoard(curBoard).get
                        memoized(Changeable(curBoard.player, curBoard.boxes)) = Finite(boxMoves, playerMoves)
                      }
                      if (boxMoves != 0) {
                        throw new IllegalStateException("Sanity check failed for boxMoves: " + boxMoves)
                      }
                      if (playerMoves != 0) {
                        throw new IllegalStateException("Sanity check failed for playerMoves: " + playerMoves)
                      }
                      d
                  }
                  memoized(key) = ret
                  print("stored; ")
                  ret
              }
            }
            val (left, right) = s.boxes.splitAt(s.boxes.size / 2)
            val dl = distanceOf(left)
            val dr = distanceOf(right)
            println()
            add(dl, dr)
          }
      }
    }
//    println("heuristic is " + ret + " for " + s.player + " and " + s.boxes)
    ret
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
          Some(board.copy(boxes = board.boxes - next + nextNext, player = next))
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
  override def toString: String = "up"
}
object Down extends Direction {
  def toPoint(p: Point): Point = p.copy(row = p.row + 1)
  override def toString: String = "down"
}
object Left extends Direction {
  def toPoint(p: Point): Point = p.copy(column = p.column - 1)
  override def toString: String = "left"
}
object Right extends Direction {
  def toPoint(p: Point): Point = p.copy(column = p.column + 1)
  override def toString: String = "right"
}
abstract class Distance
case class Finite(boxMoves: Int, playerMoves: Int) extends Distance
case class Infinite() extends Distance

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