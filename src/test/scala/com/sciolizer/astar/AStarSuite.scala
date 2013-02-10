package com.sciolizer.astar

import org.scalatest.FunSuite
import com.sciolizer.astar.AStar.{Admissable, HeuristicGuarantee, Domain}

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/9/13
 * Time: 7:11 PM
 */
class AStarSuite extends FunSuite {
  test("convert 7 to 26 by doubling or subtracting one") {
    abstract class Operation
    case class DoubleIt() extends Operation
    case class SubtractOne() extends Operation
    val domain = new Domain[Int, Operation] {
      def children(s: Int): Map[Operation, (Int, scala.Double)] = Map((DoubleIt(), (s * 2, 1.0)), SubtractOne() -> (s - 1, 1.0))

      def heuristicGuarantee: HeuristicGuarantee = Admissable()

      def heuristicFunction(s: Int): Double = {
        if (s < 26) {
          math.floor(math.log(26 / s) / math.log(2))
        } else {
          s - 26.0
        }
      }

      def isGoal(s: Int): Boolean = s == 26
    }
    assert(AStar.search(7, domain) === List(DoubleIt() /* 14 */, SubtractOne() /* 13 */, DoubleIt() /* 26 */))
  }
}
