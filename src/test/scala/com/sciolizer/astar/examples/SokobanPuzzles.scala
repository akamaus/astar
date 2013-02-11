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
    println(Sokoban.solve(Board(grid, goals, boxes, player)))
  }

  test("Microban 1") {
    solve(Point(3, 2), Set(Point(3, 1), Point(4, 3)),
      "XXXX  \n" +
      "X GX  \n" +
      "X  XXX\n" +
      "X    X\n" +
      "X    X\n" +
      "X  XXX\n" +
      "XXXX  ")
  }
}
