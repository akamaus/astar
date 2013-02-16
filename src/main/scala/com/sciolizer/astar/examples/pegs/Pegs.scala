package com.sciolizer.astar.examples.pegs

import com.sciolizer.astar.AStar
import com.sciolizer.astar.AStar.{Consistent, HeuristicGuarantee, SearchDomain}
import com.sciolizer.dfs.DepthFirst.UninformedSearch
import com.sciolizer.dfs.DepthFirst

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/12/13
 * Time: 7:13 PM
 */
class Pegs(hole: Point, pegs: Set[Point]) {

  val holes: Set[Point] = pegs + hole

  type Board = Set[Point]

  case class Move(from: Point, direction: Direction) {
    def toBoard(board: Board): Option[Board] = {
      val adjacent = direction.toPoint(from)
      if (!board.contains(adjacent)) {
        None
      } else {
        val target = direction.toPoint(adjacent)
        if (board.contains(target) || !holes.contains(target)) {
          None
        } else {
          Some(board - from - adjacent + target)
        }
      }
    }
  }

  class PegsDomain extends UninformedSearch[Board, Move] {
    def children(s: Board): Map[Move, Board] = {
      var ret: Map[Move, Board] = Map.empty
      for (peg <- s) {
        for (dir <- Direction.all) {
          val m = Move(peg, dir)
          m.toBoard(s) match {
            case None =>
            case Some(nextBoard) => ret = ret + (m -> nextBoard)
          }
        }
      }
      ret
    }

    def heuristicGuarantee: HeuristicGuarantee = Consistent()

    // actually, this is a terrible heuristic. This won't perform any
    // better than uninformed breadth-first search.
    def heuristicFunction(s: Board): Int = s.size - 1

    def isGoal(s: Board): Boolean = s.size == 1
  }

  lazy val solution: Option[List[Move]] = {
    throw new NotImplementedError()
//    DepthFirst.search(pegs, new PegsDomain()) match {
//      case None => None
//      case Some((x, _)) => Some(x)
//    }
  }
}

case class Point(row: Int, column: Int)

abstract class Direction {
  def toPoint(p: Point): Point
}
object Direction {
  val all: List[Direction] = List(Left(), Right(), Up(), Down())
}
case class Left() extends Direction {
  def toPoint(p: Point): Point = p.copy(column = p.column - 1)
}
case class Right() extends Direction {
  def toPoint(p: Point): Point = p.copy(column = p.column + 1)
}
case class Up() extends Direction {
  def toPoint(p: Point): Point = p.copy(row = p.row - 1)
}
case class Down() extends Direction {
  def toPoint(p: Point): Point = p.copy(row = p.row + 1)
}
