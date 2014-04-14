package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

object AbstractTestTest {
  def main(args: Array[String]) {

  }
}

abstract class AbstractTestTest[Params, BFn, R, RFn, B <: Builder[R, RFn, B], E <: Engine[Params, BFn, R, RFn]] extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, B, E] with EvaluateTree[Params, BFn, R, RFn] {
  def root: org.cddcore.engine.DecisionTreeNode[Params, BFn, R, RFn] = ???

  "The abstractTest class" should "allow becauses to be specified and the because means 'all these letters are in the result'" in {
    def checkBecause(because: Seed, p: Seed, expected: Boolean) {
      val bfn = becauseBfn("ABC")
      val bc = makeBecauseClosure(params(p))
      assertEquals(expected, bc(bfn))
    }
    checkBecause("ABC", "A", false)
    checkBecause("ABC", "A", false)
    checkBecause("ABC", "A", false)
    checkBecause("ABC", "ABC", true)
    checkBecause("ABC", "ABCD", true)
    checkBecause("ABC", "CABD", true)
  }

}

abstract class AbstractTest1Test[P, R] extends AbstractTestTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R], Engine1[P, R]] with Builder1Test[P, R]
abstract class AbstractTest2Test[P1, P2, R] extends AbstractTestTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R], Engine2[P1, P2, R]] with Builder2Test[P1, P2, R]
abstract class AbstractTest3Test[P1, P2, P3, R] extends AbstractTestTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R], Engine3[P1, P2, P3, R]] with Builder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class AbstractTestStringStringTest extends AbstractTest1Test[String, String] with StringStringTest with EvaluateTree1[String, String] {
  def apply(p: String): String = ???
}

@RunWith(classOf[JUnitRunner])
class AbstractTestStringStringStringTest extends AbstractTest2Test[String, String, String] with StringStringStringTest with EvaluateTree2[String, String, String] {
  def apply(p1: String, p2: String): String = ???
}

@RunWith(classOf[JUnitRunner])
class AbstractTestStringStringStringStringTest extends AbstractTest3Test[String, String, String, String] with StringStringStringStringTest with EvaluateTree3[String, String, String, String] {
  def apply(p1: String, p2: String, p3: String): String = ???
}