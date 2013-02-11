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

  trait Domain[State, Action, Measure] {
    def children(s: State): Map[Action, (State, Measure)]
    def heuristicGuarantee: HeuristicGuarantee
    def heuristicFunction(s: State): Measure
    def isGoal(s: State): Boolean
    def zero: Measure
    def add(m1: Measure, m2: Measure): Measure
    def comparator: Comparator[Measure]
  }

  def search[State, Act, Measure](start: State, domain: Domain[State, Act, Measure]): (List[Act], Measure) = {
    val search = domain.heuristicGuarantee match {
      case Admissable() => new GraphSearch()
      case Consistent() => new TreeSearch()
    }
    case class ActionWrapper(action: Act) extends Action {
      def isNoOp: Boolean = false
    }
    val memoizedCost: mutable.Map[Node, Measure] = mutable.Map.empty
    def cost(n: Node): Measure = {
      memoizedCost.get(n) match {
        case None =>
          val ret = if (n.isRootNode) {
            domain.zero
          } else {
            domain.add(cost(n.getParent), domain.children(n.getState.asInstanceOf[State])(n.getAction.asInstanceOf[ActionWrapper].action)._2)
          }
          memoizedCost(n) = ret
          ret
        case Some(x) => x
      }
    }
    def priority(n: Node): Measure = {
      domain.add(cost(n), domain.heuristicFunction(n.getState.asInstanceOf[State]))
    }
    val comparator: Comparator[Node] = new Comparator[Node] {
      def compare(o1: Node, o2: Node): Int = domain.comparator.compare(priority(o1), priority(o2))
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
      def c(s: Any, a: Action, sDelta: Any): Double = 0.0
    }
    val p = new Problem(start, actionsFunction, resultsFunction, goalTest, stepCostFunction)
    val actions = ass.search(p)
    val acts = List.empty ++ actions.map(_.asInstanceOf[ActionWrapper].action)
    var distance: Measure = domain.zero
    var state: State = start
    for (act <- acts) {
      val (next, inc) = domain.children(state)(act)
      state = next
      distance = domain.add(distance, inc)
    }
    (acts, distance)
  }
}
