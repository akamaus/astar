package com.sciolizer.astar.examples

import org.scalatest.FunSuite
import com.sciolizer.astar.AStar.{CostCalculator, MeasureDomain}
import java.util.Comparator
import aima.core.search.framework.Node
import aima.core.agent.Action

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/11/13
 * Time: 1:54 PM
 */
class SokobanSuite extends FunSuite {

  class IntMeasure extends MeasureDomain[Int] {
    def zero: Int = 0
    def add(m1: Int, m2: Int): Int = m1 + m2
    def comparator: Comparator[Int] = c
    lazy val c = new Comparator[Int] {
      def compare(o1: Int, o2: Int): Int = math.signum(o2 - o1)
    }
  }

  test("Cost adds correctly") {
    // a -- 3 --> b -- 4 --> c
    object A extends Action {
      def isNoOp: Boolean = false
    }
    val a = new Node(Unit)
    val b = new Node(Unit, a, A, 0.0)
    val c = new Node(Unit, b, A, 0.0)
    def stepCost(n: Node): Int = {
      if (n == a) {
        0
      } else if (n == b) {
        3
      } else if (n == c) {
        4
      } else {
        throw new IllegalArgumentException("Unexpected node")
      }
    }
    val cc = new CostCalculator[Int](new IntMeasure(), stepCost)
    assert(cc.cost(c) === 7)
  }
}
