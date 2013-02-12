package com.sciolizer.astar

import org.scalatest.FunSuite
import aima.core.agent.Action
import aima.core.search.framework.Node
import com.sciolizer.astar.AStar._
import com.sciolizer.astar.AStar.Admissable

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/9/13
 * Time: 7:11 PM
 */
class AStarSuite extends FunSuite {
  case class A() extends Action {
    def isNoOp: Boolean = false
  }
  test("Cost calculator involving Int.MaxValue") {
    /*       a
            / \
          10   1
          /     \
         b       c
                  \
                  MAX
                    \
                     d
     */
    val a = new Node(Unit)
    val b = new Node(Unit, a, A(), 0.0)
    val c = new Node(Unit, a, A(), 0.0)
    val d = new Node(Unit, c, A(), 0.0)
    def stepCost(n: Node): Int = {
      if (n == a) {
        0
      } else if (n == b) {
        10
      } else if (n == c) {
        1
      } else if (n == d) {
        Int.MaxValue
      } else {
        throw new IllegalArgumentException("Unexpected node: " + n)
      }
    }
    val cc = new CostCalculator[Int](new IntMeasure(), stepCost)
    assert(cc.cost(d) === Int.MaxValue)
  }

  test("Simple cost calculator test") {
    // a -- 3 --> b -- 4 --> c
    val a = new Node(Unit)
    val b = new Node(Unit, a, A(), 0.0)
    val c = new Node(Unit, b, A(), 0.0)
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

  test("Search involving Int.MaxValue") {
    /*       a
            / \
          10   1
          /     \
         b       c
                  \
                  MAX
                    \
                     d
     */
    abstract class Child
    case class First() extends Child
    case class Second() extends Child
    class TreeDomain extends IntMeasure with SearchDomain[Char, Child, Int] {
      def heuristicGuarantee: HeuristicGuarantee = Admissable()
      def heuristicFunction(s: Char): Int = 0
      def isGoal(s: Char): Boolean = s == 'b' || s == 'd'
      def children(s: Char): Map[Child, (Char, Int)] = {
        s match {
          case 'a' => Map(First() -> ('b', 10), Second() -> ('c', 1))
          case 'b' => Map()
          case 'c' => Map(First() -> ('d', Int.MaxValue))
          case 'd' => Map()
          case x => throw new IllegalArgumentException("Unexpected character: " + x)
        }
      }
    }
    assert(AStar.search('a', new TreeDomain()) === (List(First()), 10))
  }

  test("convert 7 to 26 by doubling or subtracting one") {
    abstract class Operation
    case class DoubleIt() extends Operation
    case class SubtractOne() extends Operation
    class NumberManipulation extends IntMeasure with SearchDomain[Int, Operation, Int] {
      def children(s: Int): Map[Operation, (Int, Int)] = {
        var ret: Map[Operation, (Int, Int)] = Map.empty
        if (s < 53) ret = ret + (DoubleIt() -> (s * 2, 1))
        if (s > 0) ret = ret + (SubtractOne() -> (s - 1, 1))
        ret
      }

      def heuristicGuarantee: HeuristicGuarantee = Admissable()

      def heuristicFunction(s: Int): Int = {
        val ret = if (s <= 0) {
          Int.MaxValue
        } else if (s < 26) {
          math.floor(math.log(26.0 / s) / math.log(2)).toInt
        } else {
          s - 26
        }
        println("heuristic for " + s + ": " + ret)
        ret
      }

      def isGoal(s: Int): Boolean = s == 26
    }
    val domain = new NumberManipulation()
    assert(AStar.search(7, domain) === (List(DoubleIt() /* 14 */, SubtractOne() /* 13 */, DoubleIt() /* 26 */), 3))
  }
}
