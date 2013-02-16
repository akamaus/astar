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
object Piece {
  def isEmpty(pieces: Int): Boolean = pieces == 0

  def subtract(pieces: Int, piece: Int): Int = pieces & (~piece)

  lazy val largestToSmallest =
    List(orangeLarge1, orangeLarge0, blueLarge1, blueLarge0, orangeMedium1, orangeMedium0, blueMedium1, blueMedium0,
      orangeSmall1, orangeSmall0, blueSmall1, blueSmall0)

  def largest(pieces: Int): Int = largestToSmallest.find(contains(pieces, _)).get

  def insert(pieces: Int, piece: Int): Int = pieces | piece

  def contains(pieces: Int, piece: Int): Boolean = (pieces & piece) != 0

  val blueSmall0 = 1 << 0
  val blueSmall1 = 1 << 1
  val blueMedium0 = 1 << 2
  val blueMedium1 = 1 << 3
  val blueLarge0 = 1 << 4
  val blueLarge1 = 1 << 5
  val orangeSmall0 = 1 << 6
  val orangeSmall1 = 1 << 7
  val orangeMedium0 = 1 << 8
  val orangeMedium1 = 1 << 9
  val orangeLarge0 = 1 << 10
  val orangeLarge1 = 1 << 11
  lazy val allFirst: List[Int] = List(blueSmall0, blueSmall1, blueMedium0, blueMedium1, blueLarge0, blueLarge1)
  lazy val allSecond: List[Int] = List(orangeSmall0, orangeSmall1, orangeMedium0, orangeMedium1, orangeLarge0, orangeLarge1)
  def player(piece: Int): Int = if (piece < orangeSmall0) 0 else 1
}
object State //(board: Map[(Int, Int), List[Piece]] /* indices from (0, 0) to (2, 2) */, nextPlayer: Int)
// vector of ints, length 10: 0-8 are grid places, 9th is next player
case class Action(piece: Int, where: Int)
object Action {
  lazy val allCoordinates: List[Int] = (0 until 9).toList
  lazy val allFirst: List[Action] = (for (p <- Piece.allFirst; c <- allCoordinates) yield Action(p, c)).toList
  lazy val allSecond: List[Action] = (for (p <- Piece.allSecond; c <- allCoordinates) yield Action(p, c)).toList
}
case class Player(player: Int /* 0 or 1 */)

class GobblerGame extends Game[Vector[Int], Action, Player] {
  val playerIndex: Int = 9

  def getInitialState: Vector[Int] = List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0).toVector

  def getPlayers: Array[Player] = Array(Player(0), Player(1))

  def getPlayer(state: Vector[Int]): Player = Player(state(playerIndex))

  def nextPlayer(state: Vector[Int]): Int = state(playerIndex)

  def getActions(state: Vector[Int]): util.List[Action] = {
    val actions = if (nextPlayer(state) == 0) Action.allFirst else Action.allSecond
    (for (a <- actions; Some(_) <- List(getResultOption(state, a))) yield a)
  }

//  private def unusedPieces(state: Vector[Int]): Int = {
//    12 - state.board.map(x => x._2.size).sum
//  }

  def getResult(state: Vector[Int], action: Action): Vector[Int] = getResultOption(state, action).get

  private def getResultOption(state: Vector[Int], action: Action): Option[Vector[Int]] = {
    if (Piece.player(action.piece) != nextPlayer(state)) return None
    val target = action.where
    var newState = state
    for (coordinate <- Action.allCoordinates) {
      val pieces = state(coordinate)
      if (coordinate == target) {
        if (Piece.contains(pieces, action.piece)) return None
        newState = newState.updated(coordinate, Piece.insert(pieces, action.piece))
      } else {
        if (Piece.contains(action.piece, pieces)) {
          val largest = Piece.largest(pieces)
          if (largest == action.piece) {
            newState = newState.updated(coordinate, Piece.subtract(pieces, largest))
          } else {
            return None
          }
        }
      }
    }
    Some(newState.updated(playerIndex, 1 - newState(playerIndex)))
  }

  def isTerminal(state: Vector[Int]): Boolean = {
    winner(state) match {
      case (false, false) => false
      case _ => true
    }
  }

  def getUtility(state: Vector[Int], player: Player): Double = {
    winner(state) match {
      case (false, false) => throw new IllegalArgumentException("game is not terminal")
      case (true, false) => if (player.player == 0) 1 else -1
      case (false, true) => if (player.player == 1) 1 else -1
      case (true, true) => 0
    }
  }

  private def winner(state: Vector[Int]): (Boolean, Boolean) = {
    var ret = List(false, false)
    def check(coords: List[Int]) {
      player(state, coords) match {
        case None =>
        case Some(x) => ret = ret.updated(x, true)
      }
    }
    for (row <- List(0, 3, 6)) check(List(row + 0, row + 1, row + 2))
    for (col <- 0 until 3) check(List((0 + col), (3 + col), (6 + col)))
    check(List(0, 4, 8))
    check(List(2, 4, 6))
    (ret(0), ret(1))
  }

  private def player(state: Vector[Int], cs: List[Int]): Option[Int] = {
    var ret: Option[Int] = None
    for (c <- cs) {
      val pieces = state(c)
      if (Piece.isEmpty(pieces)) return None
      val player = Piece.player(Piece.largest(pieces))
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

