package com.sciolizer.astar.examples.splice

import com.sciolizer.astar._
import collection.GenTraversableOnce
import com.sciolizer.astar.AStar.HeuristicGuarantee
import examples.splice.Splice.Child

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/9/13
 * Time: 7:37 PM
 */
object Splice {
  // Based on the puzzle game: http://www.cipherprime.com/games/splice/

  type LinkIndex = List[Child] // empty list points to the head

  abstract class Child {
    def index: Int
  }
  object Child {
    def fromIndex(i: Int): Child = {
      i match {
        case 0 => LeftChild()
        case 1 => RightChild()
        case _ => throw new IllegalArgumentException("Invalid index: " + i)
      }
    }
  }
  case class LeftChild() extends Child {
    def index = 0
  }
  case class RightChild() extends Child {
    def index = 1
  }

  def solve(start: Chain, goal: Chain): List[Modification] = {
    AStar.search(start, new SpliceDomain(goal))
  }

  class SpliceDomain(goal: Chain) extends AStar.Domain[Chain, Modification] {
    def children(s: Chain): Map[Modification, (Chain, Double)] = {
      val spots: List[LinkIndex] = s.indices.diff(List(List.empty))
      (for (
        f <- spots;
        t <- spots if f != t;
        child <- SpliceIt(f, t).apply(s).toList
        if child.isLegal)
      yield { (SpliceIt(f, t), (child, 1.0)) }
      ).toMap
    }

    def heuristicGuarantee: HeuristicGuarantee = AStar.Admissable()

    def heuristicFunction(s: Chain): Double = 0.0 // todo

    def isGoal(s: Chain): Boolean = s.equals(goal)
  }
}

abstract class Link {
  def -(that: Chain): Chain
  def single: Chain
}
case class Plain() extends Link {
  def -(that: Chain): Chain = Chain(this, List(that))
  def single: Chain = Chain(this, List())
}
//  case class Doubler() extends Link
//  case class Extender() extends Link
//  case class Deleter() extends Link

case class Chain(link: Link, children: List[Chain] /* length 0, 1, or 2 */) {
  def isLegal: Boolean = if (children.isEmpty) true else children.length <= 2 && children.forall(_.isLegal)
  def +(that: Chain): Chain = Chain(Plain(), List(this, that))
  lazy val indices: List[Splice.LinkIndex] = {
    def attach(ci: (Chain, Int)): List[Splice.LinkIndex] = ci._1.indices.map(x => (Child.fromIndex(ci._2) +: x))
    (List(List.empty) ++ children.zipWithIndex.flatMap(attach)).toList
  }
}

abstract class Modification {
  def apply(chain: Chain): Option[Chain]
}
case class SpliceIt(from: Splice.LinkIndex, to: Splice.LinkIndex) extends Modification {
  def apply(chain: Chain): Option[Chain] = {
    for (
      (without, removed) <- split(chain);
      ret <- inject(without, removed))
    yield ret
  }

  /**
   * Splits the chain at `from`.
   *
   * @return The original chain with the bit chopped off, and the bit chopped off.
   */
  def split(orig: Chain): Option[(Chain, Chain)] =  {
    if (from.isEmpty) {
      throw new IllegalArgumentException("Cannot split at empty index")
    } else {
      val index = from(0).index
      if (!orig.children.isDefinedAt(index)) {
        None
      } else {
        val removed = orig.children(index)
        if (from.length == 1) {
          val without = orig.children.diff(List(removed)) // todo: find a faster way
          Some(orig.copy(children = without), removed)
        } else {
          for ((w, r) <- copy(from = from.tail).split(removed))
            yield (orig.copy(children = orig.children.updated(index, w)), r)
        }
      }
    }
  }

  def inject(vein: Chain, blood: Chain): Option[Chain] = {
    if (to.isEmpty) {
      throw new IllegalArgumentException("Cannot inject at empty index")
    } else {
      val head = to(0)
      if (to.length == 1) {
        Some(head match {
          case Splice.LeftChild() => vein.copy(children = blood +: vein.children)
          case Splice.RightChild() => vein.copy(children = vein.children :+ blood)
        })
      } else {
        val index = head.index
        if (!vein.children.isDefinedAt(index)) {
          None
        } else {
          for (sub <- copy(to = to.tail).inject(vein.children(index), blood))
            yield vein.copy(children = vein.children.updated(index, sub))
        }
      }
    }
  }
}