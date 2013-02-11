package com.sciolizer.astar.examples

import com.sciolizer.astar.examples.splice.{SpliceIt, Plain, Chain, Splice}
import org.scalatest.FunSuite
import splice.Splice.{LeftChild, RightChild}

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/9/13
 * Time: 7:56 PM
 */
class SpliceSuite extends FunSuite {
  test("Sequence 1 strand 1") {
    val solution = Splice.solve(Chain(Plain(), List(Chain(Plain(), List(Chain(Plain(), List()))))), Chain(Plain(), List(Chain(Plain(), List()), Chain(Plain(), List()))))
    assert(solution === List(SpliceIt(List(LeftChild(), LeftChild()), List(RightChild()))))
  }
}
