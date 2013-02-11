package com.sciolizer.astar.examples.splice

import com.sciolizer.astar._
import collection.GenTraversableOnce
import com.sciolizer.astar.AStar.HeuristicGuarantee

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/9/13
 * Time: 7:37 PM
 */
class Splice {
  // Based on the puzzle game: http://www.cipherprime.com/games/splice/

}

object Splice {
  type LinkIndex = List[Child] // empty list points to the head

  abstract class Child {
    def index: Int
  }
  case class LeftChild() extends Child {
    def index = 0
  }
  case class RightChild() extends Child {
    def index = 1
  }

  def indices(chain: Chain): Iterator[Splice.LinkIndex] = {
    def attach(ci: (Chain, Int)): GenTraversableOnce[Splice.LinkIndex] = indices(ci._1).map[Splice.LinkIndex](x => (ci._2 +: x).asInstanceOf[Splice.LinkIndex])
    (Iterator.single(List()) ++ chain.children.zipWithIndex.flatMap(attach)).toIterator
  }

  def solve(start: Chain, goal: Chain): List[Modification] = {
    AStar.search(start, new SpliceDomain(goal))
  }

  class SpliceDomain(goal: Chain) extends AStar.Domain[Chain, Modification] {
    def children(s: Chain): Map[Modification, (Chain, Double)] = {
      val spots = indices(s)
      (for (f <- spots; t <- spots) yield {
        val mod = SpliceIt(f, t)
        val child = mod.apply(s)
        if (child.isLegal) Set((mod, (child, 1.0))) else Set.empty
      }).flatMap[(Modification, (Chain, Double))](x => x).toMap
    }

    def heuristicGuarantee: HeuristicGuarantee = AStar.Admissable()

    def heuristicFunction(s: Chain): Double = 0.0 // todo

    def isGoal(s: Chain): Boolean = s.equals(goal)
  }
}

abstract class Link
case class Plain() extends Link
//  case class Doubler() extends Link
//  case class Extender() extends Link
//  case class Deleter() extends Link

case class Chain(link: Link, children: List[Chain] /* length 0, 1, or 2 */) {
  def isLegal: Boolean = if (children.isEmpty) true else children.length <= 2 && children.forall(_.isLegal)
}

abstract class Modification {
  def apply(chain: Chain): Chain
}
case class SpliceIt(from: Splice.LinkIndex, to: Splice.LinkIndex) extends Modification {
  def apply(chain: Chain): Chain = {
    val (without, removed) = split(chain, from)
    inject(without, to, removed)
  }

  /**
   * Splits the chain at `from`.
   *
   * @return The original chain with the bit chopped off, and the bit chopped off.
   */
  private def split(orig: Chain, at: List[Splice.Child]): (Chain, Chain) =  {
    if (at.isEmpty) {
      throw new IllegalArgumentException("Cannot split at empty index")
    } else {
      val index = at(0).index
      val removed = orig.children(index)
      if (at.length == 1) {
        val without = orig.children.diff(List(removed)) // todo: find a faster way
        (orig.copy(children = without), removed)
      } else {
        val (w, r) = split(removed, at.tail)
        (orig.copy(children = orig.children.updated(index, w)), r)
      }
    }
  }

  private def inject(vein: Chain, at: List[Splice.Child], blood: Chain): Chain = {
    if (at.isEmpty) {
      throw new IllegalArgumentException("Cannot inject at empty index")
    } else {
      val head = at(0)
      if (at.length == 1) {
        head match {
          case Splice.LeftChild() => vein.copy(children = blood +: vein.children)
          case Splice.RightChild() => vein.copy(children = vein.children :+ blood)
        }
      } else {
        val index = head.index
        val sub = inject(vein.children(index), at.tail, blood)
        vein.copy(children = vein.children.updated(index, sub))
      }
    }
  }
}