package com.sciolizer.astar

import aima.core.search.informed.AStarSearch
import aima.core.search.framework._
import java.util
import aima.core.agent
import agent.Action
import collection.JavaConversions._
import collection.mutable
import util.Comparator

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/9/13
 * Time: 6:42 PM
 */

object AStar {
  case class Admissable() extends HeuristicGuarantee { }
  case class Consistent() extends HeuristicGuarantee { }
  abstract class HeuristicGuarantee { }

  trait MeasureDomain[Measure] {
    def zero: Measure
    def add(m1: Measure, m2: Measure): Measure
    def comparator: Comparator[Measure]
  }

  class IntMeasure extends MeasureDomain[Int] {
    def zero: Int = 0
    // We don't detect overflow, but we do treat Int.MaxValue like infinity.
    def add(m1: Int, m2: Int): Int = if (m1 == Int.MaxValue || m2 == Int.MaxValue) Int.MaxValue else m1 + m2
    def comparator: Comparator[Int] = c
    lazy val c = new Comparator[Int] {
      def compare(o1: Int, o2: Int): Int = math.signum(o1 - o2)
    }
  }

  trait SearchDomain[State, Action, Measure] extends MeasureDomain[Measure] {
    def children(s: State): Map[Action, (State, Measure)]
    def heuristicGuarantee: HeuristicGuarantee
    def heuristicFunction(s: State): Measure
    def isGoal(s: State): Boolean
  }

  def search[State, Act, Measure](start: State, domain: SearchDomain[State, Act, Measure]): Option[(List[Act], Measure)] = {
    case class ActionWrapper(action: Act) extends Action {
      def isNoOp: Boolean = false
    }
    def incCost(n: Node): Measure = {
      val action: Act = n.getAction.asInstanceOf[ActionWrapper].action
      val children: Map[Act, (State, Measure)] = domain.children(n.getParent.getState.asInstanceOf[State])
      children(action)._2
    }
    val costCalculator = new CostCalculator[Measure](domain, incCost)
    def priority(n: Node): Measure = {
      val ret = domain.add(costCalculator.cost(n), domain.heuristicFunction(n.getState.asInstanceOf[State]))
//      println("priority " + nodeToString(n) + ": " + ret)
      val path: util.List[Node] = n.getPathFromRoot
      println("path from root: " + path)
      def doesSomething(n: Node): Boolean = {
        val action: Action = n.getAction
//        if (action == null) {
//          throw new NullPointerException("Action is null")
//        }
        action != null && !action.isNoOp
      }
      val filteredPath: List[Node] = path.toList.filter(doesSomething)
      println("Priority is " + ret + " for " + filteredPath.map(_.getAction.asInstanceOf[ActionWrapper].action))
      ret
    }
    val comparator: Comparator[Node] = new Comparator[Node] {
      def compare(o1: Node, o2: Node): Int = {
        val p1: Measure = priority(o1)
        val p2: Measure = priority(o2)
        val ret = domain.comparator.compare(p1, p2)
//        println("comparing " + p1 + " and " + p2 + ": " + ret)
        ret
      }
    }
    val search = domain.heuristicGuarantee match {
      case Admissable() => {
        val ret = new GraphSearch() {
          override def expandNode(node: Node, problem: Problem): util.List[Node] = {
            val ret = super.expandNode(node, problem)
//            println("expansion of " + nodeToString(node) + ": " + ret.map(nodeToString(_)))
            ret
          }
        }
//        ret.setReplaceFrontierNodeAtStateCostFunction(comparator) // This is already done by PrioritySearch
        ret
      }
      case Consistent() => new TreeSearch()
    }
    val ass = new PrioritySearch(search, comparator)
    val actionsFunction = new ActionsFunction {
      def actions(s: Any): util.Set[agent.Action] =
       domain.children(s.asInstanceOf[State]).keySet.map[ActionWrapper, Set[agent.Action]](new ActionWrapper(_))
    }
    val resultsFunction = new ResultFunction {
      def result(s: Any, a: Action): AnyRef =
        domain.children(s.asInstanceOf[State])(a.asInstanceOf[ActionWrapper].action)._1.asInstanceOf[AnyRef]
    }
    val goalTest = new GoalTest {
      def isGoalState(state: Any): Boolean = domain.isGoal(state.asInstanceOf[State])
    }
    // The result of this function is actually ignored by the PriorityQueue, which only
    // uses the comparator to pop items off. Even QueueSearch ignores the value except
    // for tracking metrics, which we aren't using anyway.
    // Just don't call Node.pathCost, since it will always be 0.0
    val stepCostFunction = new StepCostFunction {
      var limit = 100
      def c(s: Any, a: Action, sDelta: Any): Double = {
        limit -= 1
        if (limit <= 0) {
          throw new RuntimeException("search ran for too long")
        }
        0.0
      }
    }
    val p = new Problem(start, actionsFunction, resultsFunction, goalTest, stepCostFunction)
    val actions = ass.search(p)
    if (actions.isEmpty) {
      None
    } else {
      val acts = List.empty ++ actions.filter(!_.isNoOp).map(_.asInstanceOf[ActionWrapper].action)
      var distance: Measure = domain.zero
      var state: State = start
      for (act <- acts) {
        val (next, inc) = domain.children(state)(act)
        state = next
        distance = domain.add(distance, inc)
      }
      Some((acts, distance))
    }
  }

  def nodeToString(n: Node): String = n.getPathFromRoot().map(_.getState()).toString()

  class CostCalculator[Measure](domain: MeasureDomain[Measure], incCost: Node => Measure) {
    val memoizedCost: mutable.Map[Node, Measure] = mutable.Map.empty
    def cost(n: Node): Measure = {
      val ret = memoizedCost.get(n) match {
        case None =>
          val ret = if (n.isRootNode) {
            domain.zero
          } else {
            domain.add(cost(n.getParent), incCost(n))
          }
          memoizedCost(n) = ret
          ret
        case Some(x) => x
      }
//      println(nodeToString(n) + ": " + ret)
      ret
    }
  }
}
