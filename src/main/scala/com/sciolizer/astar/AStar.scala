package com.sciolizer.astar

import aima.core.search.informed.AStarSearch
import aima.core.search.framework._
import java.util
import aima.core.agent
import agent.Action
import collection.JavaConversions._

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

  trait Domain[State, Action] {
    def children(s: State): Map[Action, (State, Double)]
    def heuristicGuarantee: HeuristicGuarantee
    def heuristicFunction(s: State): Double
    def isGoal(s: State): Boolean
  }

  def search[State, Act](start: State, domain: Domain[State, Act]): List[Act] = {
    val search = domain.heuristicGuarantee match {
      case Admissable() => new GraphSearch()
      case Consistent() => new TreeSearch()
    }
    val hf = new HeuristicFunction {
      def h(state: Any): Double = domain.heuristicFunction(state.asInstanceOf[State])
    }
    val ass = new AStarSearch(search, hf)
    case class ActionWrapper(action: Act) extends Action {
      def isNoOp: Boolean = false
    }
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
    val stepCostFunction = new StepCostFunction {
      def c(s: Any, a: Action, sDelta: Any): Double = {
        val (expected, ret) = domain.children(s.asInstanceOf[State])(a.asInstanceOf[ActionWrapper].action)
        if (!expected.equals(sDelta)) {
          throw new RuntimeException("AIMA astart is giving me impossible data")
        }
        ret
      }
    }
    val p = new Problem(start.asInstanceOf[State], actionsFunction, resultsFunction, goalTest, stepCostFunction)
    val actions = ass.search(p)
    List.empty ++ actions.map(_.asInstanceOf[ActionWrapper].action)
  }
}
