package com.sciolizer.alphabeta

import aima.core.search.adversarial.{IterativeDeepeningAlphaBetaSearch, AlphaBetaSearch, Game}
import java.util
import collection.JavaConversions._

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/16/13
 * Time: 8:40 AM
 */
case class Piece(player: Int /* 0 or 1 */, size: Int /* 0-smallest, 1, or 2-largest */, which: Int /* 0 or 1 */)
object Piece {
  lazy val allFirst: Set[Piece] = (for (s <- 0 until 3; w <- 0 until 2) yield Piece(0, s, w)).toSet
  lazy val allSecond: Set[Piece] = (for (s <- 0 until 3; w <- 0 until 2) yield Piece(1, s, w)).toSet
}
case class Square(pieces: List[Piece] /* from largest to smallest */)
case class Action(piece: Piece, where: (Int, Int))
object Action {
  lazy val allCoordinates: List[(Int, Int)] = (for (r <- 0 until 3; c <- 0 until 3) yield (r, c)).toList
  lazy val allFirst: Set[Action] = (for (p <- Piece.allFirst; c <- allCoordinates) yield Action(p, c)).toSet
  lazy val allSecond: Set[Action] = (for (p <- Piece.allSecond; c <- allCoordinates) yield Action(p, c)).toSet
}
case class Player(player: Int /* 0 or 1 */)

class State(val board: Map[(Int, Int), Square] /* indices from (0, 0) to (2, 2) */,
            val nextPlayer: Int,
//            firstPieces: Set[Piece],
//            secondPieces: Set[Piece],
            covered: Set[Piece],
            inPlayOnTop: Set[Piece],
            unused: Set[Piece]) {

  def move(a: Action): Option[State] = {
    val coord = a.where
    val orig = board(coord).pieces
    val (c, ip) = if (orig.isEmpty) (covered, inPlayOnTop) else {
      val head = orig.head
      if (head.size >= a.piece.size) return None
      (covered + head, inPlayOnTop - head)
    }
    Some(new State(board.updated(coord, Square(a.piece +: orig)), 1 - nextPlayer, c, ip, unused - a.piece))
  }

  def moveable: Iterator[Piece] = (inPlayOnTop.iterator ++ unused.iterator).filter(_.player == nextPlayer)

}

class GobblerGame extends Game[State, Action, Player] {
  def getInitialState: State =
    new State(Action.allCoordinates.map(x => x -> Square(List.empty)).toMap, 0, Set.empty, Set.empty, Piece.allFirst ++ Piece.allSecond)

  def getPlayers: Array[Player] = Array(Player(0), Player(1))

  def getPlayer(state: State): Player = Player(state.nextPlayer)

  def getActions(state: State): util.List[Action] = {
    (for (p <- state.moveable; c <- Action.allCoordinates; Some(_) <- List(state.move(Action(p, c)))) yield Action(p, c)).toList
  }

  def getResult(state: State, action: Action): State = state.move(action).get

  def isTerminal(state: State): Boolean = {
    winner(state) match {
      case (false, false) => false
      case _ => true
    }
  }

  def getUtility(state: State, player: Player): Double = {
    winner(state) match {
      case (false, false) => throw new IllegalArgumentException("game is not terminal")
      case (true, false) => if (player.player == 0) 1 else -1
      case (false, true) => if (player.player == 1) 1 else -1
      case (true, true) => 0
    }
  }

  private def winner(state: State): (Boolean, Boolean) = {
    var ret = List(false, false)
    def check(coords: List[(Int, Int)]) {
      player(state, coords) match {
        case None =>
        case Some(x) =>
          ret = ret.updated(x, true)
      }
    }
    for (row <- 0 until 3) check(List((row, 0), (row, 1), (row, 2)))
    for (col <- 0 until 3) check(List((0, col), (1, col), (2, col)))
    check(List((0, 0), (1, 1), (2, 2)))
    check(List((0, 2), (1, 1), (2, 0)))
    (ret(0), ret(1))
  }

  private def player(state: State, cs: List[(Int, Int)]): Option[Int] = {
    var ret: Option[Int] = None
    for (c <- cs) {
      val pieces = state.board(c).pieces
      if (pieces.isEmpty) return None
      val player = pieces.head.player
      if (ret == Some(1 - player)) {
        return None
      }
      ret = Some(player)
    }
    ret
  }
}

object Gobbler {
  def main(args: Array[String]) {
    val gg = new GobblerGame()
    val ab = IterativeDeepeningAlphaBetaSearch.createFor(gg, -1.0, 1.0, 5)
//    val middle = gg.getResult(gg.getInitialState, Action(Piece(0, 0, 0), (0, 1)))
//    println(middle)
    println(ab.makeDecision(gg.getInitialState))
    println(ab.getMetrics)
  }
}

