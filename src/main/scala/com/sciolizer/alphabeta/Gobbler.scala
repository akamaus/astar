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
  lazy val allFirst: List[Piece] = (for (s <- 0 until 3; w <- 0 until 2) yield Piece(0, s, w)).toList
  lazy val allSecond: List[Piece] = (for (s <- 0 until 3; w <- 0 until 2) yield Piece(1, s, w)).toList
}
case class State(board: Map[(Int, Int), List[Piece]] /* indices from (0, 0) to (2, 2) */, nextPlayer: Int)
case class Action(piece: Piece, where: (Int, Int))
object Action {
  lazy val allCoordinates: List[(Int, Int)] = (for (r <- 0 until 3; c <- 0 until 3) yield (r, c)).toList
  lazy val allFirst: List[Action] = (for (p <- Piece.allFirst; c <- allCoordinates) yield Action(p, c)).toList
  lazy val allSecond: List[Action] = (for (p <- Piece.allSecond; c <- allCoordinates) yield Action(p, c)).toList
}
case class Player(player: Int /* 0 or 1 */)

class GobblerGame extends Game[State, Action, Player] {
  def getInitialState: State =
    State(Action.allCoordinates.map(x => x -> List.empty).toMap, 0)

  def getPlayers: Array[Player] = Array(Player(0), Player(1))

  def getPlayer(state: State): Player = Player(state.nextPlayer)

  def getActions(state: State): util.List[Action] = {
    val actions = if (state.nextPlayer == 0) Action.allFirst else Action.allSecond
    (for (a <- actions; Some(_) <- List(getResultOption(state, a))) yield a)
  }

  private def unusedPieces(state: State): Int = {
    12 - state.board.map(x => x._2.size).sum
  }

  def getResult(state: State, action: Action): State = getResultOption(state, action).get

  private def getResultOption(state: State, action: Action): Option[State] = {
    if (action.piece.player != state.nextPlayer) return None
    val target = action.where
    var board = state.board
    for (coordinate <- Action.allCoordinates) {
      val pieces = state.board(coordinate)
      if (coordinate == target) {
        if (pieces.contains(action.piece)) return None
        board = board.updated(coordinate, action.piece +: pieces)
      } else {
        if (pieces.contains(action.piece)) {
          if (pieces.head == action.piece) {
            board = board.updated(coordinate, pieces.tail)
          } else {
            return None
          }
        }
      }
    }
    Some(State(board, 1 - state.nextPlayer))
  }

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
        case Some(x) => ret = ret.updated(x, true)
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
      val pieces = state.board(c)
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

