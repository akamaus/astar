package com.sciolizer.astar.examples

import org.scalatest.FunSuite
import com.sciolizer.astar.AStar.{CostCalculator, MeasureDomain}
import java.util.Comparator
import aima.core.search.framework.Node
import aima.core.agent.Action
import sokoban._
import sokoban.Board
import sokoban.Point

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/11/13
 * Time: 1:54 PM
 */
class SokobanSuite extends FunSuite {
  val (microban1Board, microban1Goals) = SokobanSuite.makeGrid(
    "XXXX  \n" +
    "X GX  \n" +
    "X  XXX\n" +
    "XG   X\n" +
    "X    X\n" +
    "X  XXX\n" +
    "XXXX  ")
  /*
  test("Distance map") {
    println("grid size: " + grid.size)
    val distanceMap: Map[Point, Int] = Sokoban.makeDistanceMap(microban1Board, microban1Goals)
    for (r <- 0 until 7) {
      for (c <- 0 until 6) {
        print(distanceMap(Point(r, c)) + "\t")
      }
      println()
    }
    println()
    println(distanceMap)
  }            */

  test("Move down") {
    val next = Down.toBoard(Board(microban1Board, microban1Goals, Set(), Point(3, 2))).get
    assert(next.player === Point(4, 2))
  }

  test("Push box down") {
    val next = Down.toBoard(Board(microban1Board, microban1Goals, Set(Point(4, 2)), Point(3, 2))).get
    assert(next.player === Point(4, 2))
    assert(next.boxes === Set(Point(5, 2)))
  }

  test("Move into wall") {
    val next = Down.toBoard(Board(microban1Board, microban1Goals, Set(), Point(5, 2)))
    assert(next === None)
  }

  test("Push box into wall") {
    val next = Down.toBoard(Board(microban1Board, microban1Goals, Set(Point(5, 2)), Point(4, 2)))
    assert(next === None)
  }

  test("Push box into box") {
    val next = Down.toBoard(Board(microban1Board, microban1Goals, Set(Point(4, 2), Point(5, 2)), Point(3, 2)))
    assert(next === None)
  }

  test("Half of microban1 solution") {
    val init = Board(microban1Board, microban1Goals, Set(Point(4, 3)), Point(3, 2))
    val end = List(Right, Right, Down, Left, Left, Down, Left, Up).foldLeft(init)((b, a) => a.toBoard(b).get)
    assert(end.boxes === Set(Point(3, 1)))
    assert(end.player === Point(4, 1))
  }
}

object SokobanSuite {
  def makeGrid(map: String): (Vector[Vector[Square]], Set[Point]) = {
    var grid: Vector[Vector[Square]] = Vector()
    var row: Vector[Square] = Vector()
    var goals: Set[Point] = Set()
    var r = 0
    var c = 0
    for (ch <- map.toCharArray) {
      if (ch == '\n') {
        grid = grid :+ row
        row = Vector()
        r += 1
        c = 0
      } else {
        if (ch == 'X') {
          row = row :+ Wall
        } else {
          row = row :+ Blank
          if (ch == 'G') {
            goals = goals + Point(r, c)
          } else if (ch != ' ') {
            throw new IllegalArgumentException("Unexpected character in map: " + ch)
          }
        }
        c += 1
      }
    }
    grid = grid :+ row
    (grid, goals)
  }
}