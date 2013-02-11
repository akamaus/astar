package com.sciolizer.astar.examples

import org.scalatest.FunSuite
import splice.Splice.{RightChild, LeftChild}
import splice.{SpliceIt, Plain, Chain, Splice}

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/10/13
 * Time: 4:24 PM
 */
class SplicePuzzles extends FunSuite {
  test("Sequence 1 strand 1") {
    val solution = Splice.solve(Chain(Plain(), List(Chain(Plain(), List(Chain(Plain(), List()))))), Chain(Plain(), List(Chain(Plain(), List()), Chain(Plain(), List()))))
    assert(solution === List(SpliceIt(List(LeftChild(), LeftChild()), List(RightChild()))))
  }
}
