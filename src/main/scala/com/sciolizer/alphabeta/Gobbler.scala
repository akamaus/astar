package com.sciolizer.alphabeta

import aima.core.search.adversarial.{IterativeDeepeningAlphaBetaSearch, AlphaBetaSearch, Game}
import java.util
import collection.JavaConversions._
import collection.mutable
import scala.math.Ordering.Implicits._
import collection.immutable.TreeSet

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/16/13
 * Time: 8:40 AM
 */
object Piece {
  def name(piece: Int): String = {
    if (piece == blueSmall0) return "blueSmall0"
    if (piece == blueSmall1) return "blueSmall1"
    if (piece == blueMedium0) return "blueMedium0"
    if (piece == blueMedium1) return "blueMedium1"
    if (piece == blueLarge0) return "blueLarge0"
    if (piece == blueLarge1) return "blueLarge1"
    if (piece == orangeSmall0) return "orangeSmall0"
    if (piece == orangeSmall1) return "orangeSmall1"
    if (piece == orangeMedium0) return "orangeMedium0"
    if (piece == orangeMedium1) return "orangeMedium1"
    if (piece == orangeLarge0) return "orangeLarge0"
    if (piece == orangeLarge1) return "orangeLarge1"
    throw new IllegalArgumentException("not a valid piece: " + piece)
  }

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
  lazy val allFirst: Set[Int] = Set(blueSmall0, blueSmall1, blueMedium0, blueMedium1, blueLarge0, blueLarge1)
  lazy val allSecond: Set[Int] = Set(orangeSmall0, orangeSmall1, orangeMedium0, orangeMedium1, orangeLarge0, orangeLarge1)
  def player(piece: Int): Int = if (piece < orangeSmall0) 0 else 1
}
object State //(board: Map[(Int, Int), List[Piece]] /* indices from (0, 0) to (2, 2) */, nextPlayer: Int)
// vector of ints, length 10: 0-8 are grid places, 9th is next player
case class Action(piece: Int, where: Int) {
  override def toString: String = {
    "Action(" + Piece.name(piece) + ", " + (where / 3, where % 3) + ")"
  }
}
object Action {
  lazy val allCoordinates: List[Int] = (0 until 9).toList
  lazy val allFirst: List[Action] = (for (p <- Piece.allFirst; c <- allCoordinates) yield Action(p, c)).toList
  lazy val allSecond: List[Action] = (for (p <- Piece.allSecond; c <- allCoordinates) yield Action(p, c)).toList
}
case class Player(player: Int /* 0 or 1 */)
case class CanonizedMove(originalMove: Action, canonical: List[Int]) {
  override def hashCode(): Int = canonical.hashCode()
  override def equals(obj: Any): Boolean = canonical == obj.asInstanceOf[CanonizedMove].canonical
}
class BoardCanon {
  val memo: mutable.Map[List[Int], List[Int]] = mutable.Map.empty
  def canonical(board: List[Int]): List[Int] = {
    memo.get(board) match {
      case Some(x) =>
        x
      case None =>
        val boards = List(
          board,
          apply(board, BoardCanon.rotate90),
          apply(board, BoardCanon.rotate180),
          apply(board, BoardCanon.rotate270),
          apply(board, BoardCanon.reflect),
          apply(apply(board, BoardCanon.rotate90), BoardCanon.reflect),
          apply(apply(board, BoardCanon.rotate180), BoardCanon.reflect),
          apply(apply(board, BoardCanon.rotate270), BoardCanon.reflect)
        )
        val canonical = boards.sorted.head
        for (modded <- boards) {
          memo(modded) = canonical
        }
        canonical
    }
  }

  private def apply(board: List[Int], change: List[Int]): List[Int] = {
    var ret: List[Int] = List.empty
    for (ind <- change) {
      ret = ret :+ board(ind)
    }
    ret
  }
}
object BoardCanon {
  val rotate90: List[Int] = List(2, 5, 8, 1, 4, 7, 0, 3, 6)
  val rotate180: List[Int] = List(8, 7, 6, 5, 4, 3, 2, 1, 0)
  val rotate270: List[Int] = List(6, 3, 0, 7, 4, 1, 8, 5, 2)
  val reflect: List[Int] = List(0, 3, 6, 1, 4, 7, 2, 5, 8)
}

class GobblerGame extends Game[Array[Int], Action, Player] {
  val playerIndex: Int = 9

  def getInitialState: Array[Int] = List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0).toArray

  def getPlayers: Array[Player] = Array(Player(0), Player(1))

  def getPlayer(state: Array[Int]): Player = Player(state(playerIndex))

  def nextPlayer(state: Array[Int]): Int = state(playerIndex)
  
  def isUnused(state: Array[Int], piece: Int): Boolean = {
    (0 until 9).forall(x => !Piece.contains(state(x), piece))
  }
  
  def getMoveablePieces(state: Array[Int], player: Int): Iterator[Int] = {
    var ret = if (player == 0) Piece.allFirst else Piece.allSecond
    if (player == 0) {
      if (isUnused(state, Piece.blueSmall0)) ret -= Piece.blueSmall1
      if (isUnused(state, Piece.blueMedium0)) ret -= Piece.blueMedium1
      if (isUnused(state, Piece.blueLarge0)) ret -= Piece.blueLarge1
    } else {
      if (isUnused(state, Piece.orangeSmall0)) ret -= Piece.orangeSmall1
      if (isUnused(state, Piece.orangeMedium0)) ret -= Piece.orangeMedium1
      if (isUnused(state, Piece.orangeLarge0)) ret -= Piece.orangeLarge1
    }
    ret.iterator
  }

  def getActions(state: Array[Int]): util.List[Action] = {
    val bc = new BoardCanon()
    val canonized: Set[CanonizedMove] = (for (p <- getMoveablePieces(state, state(playerIndex));
          c <- Action.allCoordinates;
          Some(b) <- List(getResultOption(state, Action(p, c)))) yield CanonizedMove(Action(p, c), bc.canonical(b.toList))).toSet
    canonized.toList.map(_.originalMove)
  }

//  private def unusedPieces(state: Array[Int]): Int = {
//    12 - state.board.map(x => x._2.size).sum
//  }

  def getResult(state: Array[Int], action: Action): Array[Int] = getResultOption(state, action).get

  private def getResultOption(state: Array[Int], action: Action): Option[Array[Int]] = {
    if (Piece.player(action.piece) != nextPlayer(state)) return None
    val target = action.where
    var newState = state.clone()
    for (coordinate <- Action.allCoordinates) {
      val pieces = state(coordinate)
      if (coordinate == target) {
        if (Piece.contains(pieces, action.piece)) return None // skip no-ops
        newState(coordinate) = Piece.insert(pieces, action.piece)
      } else {
        if (Piece.contains(action.piece, pieces)) {
          val largest = Piece.largest(pieces)
          if (largest == action.piece) {
            newState(coordinate) = Piece.subtract(pieces, largest)
          } else {
            return None
          }
        }
      }
    }
    newState(playerIndex) = 1 - newState(playerIndex)
    Some(newState)
  }

  def isTerminal(state: Array[Int]): Boolean = {
    winner(state) match {
      case (false, false) => false
      case _ => true
    }
  }

  def getUtility(state: Array[Int], player: Player): Double = {
    winner(state) match {
      case (false, false) => throw new IllegalArgumentException("game is not terminal")
      case (true, false) => if (player.player == 0) 1 else -1
      case (false, true) => if (player.player == 1) 1 else -1
      case (true, true) => 0
    }
  }

  private def winner(state: Array[Int]): (Boolean, Boolean) = {
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

  private def player(state: Array[Int], cs: List[Int]): Option[Int] = {
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

