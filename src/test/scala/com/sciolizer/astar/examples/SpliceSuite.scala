package com.sciolizer.astar.examples

import org.scalatest.FunSuite
import splice.{Modification, Chain, SpliceIt, Plain}
import splice.Splice.{LeftChild, RightChild}

/**
 * Created with IntelliJ IDEA.
 * User: jball
 * Date: 2/9/13
 * Time: 7:56 PM
 */
class SpliceSuite extends FunSuite {

  /*
            P
           / \
          P   P
             /
            P
   */
  val chain1 = Plain().single + (Plain() - Plain().single)

  test("There are four indicies on a four-part chain") {
    assert(chain1.indices.toSet === Set(List(), List(LeftChild()), List(RightChild()), List(RightChild(), LeftChild())))
  }

  test("Splice right to left") {
    val from = List(RightChild(), LeftChild())
    val to   = List(LeftChild(), LeftChild())
    val expected = (Plain() - Plain().single) + Plain().single
    val actual = SpliceIt(from, to).apply(chain1)
    assert(actual === Some(expected))
  }

  test("Splice from nowhere") {
    val from = List(LeftChild(), LeftChild())
    assert(SpliceIt(from, from).apply(chain1) === None)
  }

  test("Splice from empty") {
    val from = List()
    intercept[IllegalArgumentException] {
      SpliceIt(from, from).apply(chain1)
    }
  }

  test("Splice to nowhere") {
    val from = List(RightChild(), LeftChild())
    assert(SpliceIt(from, List(LeftChild(), LeftChild(), LeftChild())).apply(chain1) === None)
  }

  test("Splice to empty") {
    val from = List(RightChild(), LeftChild())
    intercept[IllegalArgumentException] {
      SpliceIt(from, List()).apply(chain1)
    }
  }
    /*
  test("No legal moves implies empty children") {
    val domain = new SpliceDomain(chain1)
    assert(domain.children(Plain() - Plain().single) === Map())
  }   */

  test("Indices") {
    assert(chain1.indices.toSet ===
      Set(
        List(),
        List(LeftChild()),
        List(RightChild()),
        List(RightChild(), LeftChild())
      ))
  }

  test("Extended indices") {
    assert(chain1.extendedIndices.toSet ===
      Set(
        List(),
        List(LeftChild()),
        List(LeftChild(), LeftChild()),
        List(RightChild()),
        List(RightChild(), LeftChild()),
        List(RightChild(), LeftChild(), LeftChild()),
        List(RightChild(), RightChild())))
  }
            /*
  test("Legal moves for V") {
    val domain = new SpliceDomain(chain1)
    val three: Chain = Plain() - (Plain() - Plain().single)
    val v: Chain = Plain().single + Plain().single
    val expected: Map[Modification, (Chain, Double)] = Map(
      SpliceIt(List(LeftChild()), List(RightChild(), LeftChild())) -> (three, 1.0),
      SpliceIt(List(RightChild()), List(LeftChild(), LeftChild())) -> (three, 1.0),
      SpliceIt(List(LeftChild()), List(RightChild())) -> (v, 1.0),
      SpliceIt(List(RightChild()), List(LeftChild())) -> (v, 1.0))
    assert(domain.children(v) === expected)
  }         */
}
