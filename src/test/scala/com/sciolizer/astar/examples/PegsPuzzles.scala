package com.sciolizer.astar.examples

import org.scalatest.FunSuite
import pegs.{Pegs, Point}

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/12/13
 * Time: 7:43 PM
 */
class PegsPuzzles extends FunSuite {
  def makeBoard(grid: String): Set[Point] = {
    var ret: Set[Point] = Set.empty
    var row = 0
    var col = 0
    for (c <- grid.toCharArray) {
      if (c == '\n') {
        row += 1
        col = 0
      } else {
        if (c == 'X') {
          ret = ret + Point(row, col)
        } else if (c != ' ') {
          throw new IllegalArgumentException("Unexpected character: " + c)
        }
        col += 1
      }
    }
    ret
  }

  test("cross") {
    val board = makeBoard(
        "  XXX  \n" +
        "  XXX  \n" +
        "XXXXXXX\n" +
        "XXX XXX\n" +
        "XXXXXXX\n" +
        "  XXX  \n" +
        "  XXX  ")
    println(new Pegs(Point(3, 3), board).solution)
  }
}
