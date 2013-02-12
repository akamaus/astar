package com.sciolizer.astar.examples

import org.scalatest.FunSuite
import sokoban._
import sokoban.Point

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/11/13
 * Time: 12:50 PM
 */
class SokobanPuzzles extends FunSuite {

  def solve(player: Point, boxes: Set[Point], map: String) {
    val (grid, goals) = SokobanSuite.makeGrid(map)
    println(Sokoban.solve(Board(grid, goals, boxes, player)))
  }

  test("Microban 1") {
    solve(Point(3, 2), Set(Point(3, 1), Point(4, 3)), microban1Board)
  }

  test("Microban half of 1") {
    solve(Point(3, 2), Set(Point(4, 3)), microban1Board)
    // This doesn't make sense: Priority is Distance(5,3) for List(right, right, down, left, up, left, down)
    // This puts the box in the right side of the bottommost row, which, even though it's immovable, should
    // be calculated as 3 moves away from a goal, not 5 moves. Also, player moves should ALWAYS be > than box moves.

    // Priority is Finite(3,3) for List(right, right, down, left, left, down, left, up, right, up, up, left)
    // ^ This doesn't make any sense. That's a SOLUTION!
  }

  val microban1Board =
    "XXXX  \n" +
    "X GX  \n" +
    "X  XXX\n" +
    "XG   X\n" +
    "X    X\n" +
    "X  XXX\n" +
    "XXXX  "
}
