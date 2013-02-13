package com.sciolizer.dfs

import aima.core.search.uninformed.DepthFirstSearch
import aima.core.search.framework._
import java.util
import aima.core.agent.Action
import collection.JavaConversions._
import aima.core.agent

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/12/13
 * Time: 7:51 PM
 */
object DepthFirst {
  trait UninformedSearch[State, Act] {
    def children(s: State): Map[Act, State]
    def isGoal(s: State): Boolean
  }

  case class ActionWrapper[Act](act: Act) extends Action {
    def isNoOp: Boolean = false
  }

  def search[State, Act](start: State, searchDomain: UninformedSearch[State, Act]): Option[List[Act]] = {
    val actionsFunction = new ActionsFunction {
      def actions(s: Any): util.Set[Action] = searchDomain.children(s.asInstanceOf[State]).keySet.map[ActionWrapper, Set[agent.Action]](ActionWrapper(_))
    }
    val resultFunction = new ResultFunction {
      def result(s: Any, a: Action): AnyRef = searchDomain.children(s.asInstanceOf[State])(a.asInstanceOf[ActionWrapper].act)
    }
    val goalTest = new GoalTest {
      def isGoalState(state: Any): Boolean = searchDomain.isGoal(state.asInstanceOf[State])
    }
    val problem = new Problem(start, actionsFunction, resultFunction, goalTest)
    val dfs = new DepthFirstSearch(new TreeSearch())
    val ret = dfs.search(problem)
    if (ret.isEmpty) {
      None
    } else {
      Some(ret.map(_.asInstanceOf[ActionWrapper].act))
    }
  }
}
