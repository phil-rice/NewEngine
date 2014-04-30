package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine.builder._

@RunWith(classOf[JUnitRunner])
abstract class EngineAssertionTest[Params, BFn, R, RFn, B <: Builder[Params, BFn, R, RFn, R, B, E], E <: EngineTools[Params, BFn, R, RFn]] extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, R, B, E] {

  def checkAssertionExecuted(times: Int) {
    var count = 0
    resetBuilder
    scenario("A")
    update(_.expected(result("X")))
    assertion { count += 1; true }
    assertEquals(0, count)
    build
    assertEquals(times, count)
  }
  "Assertions" should "be executed once when testing" in {
    Engine.test { checkAssertionExecuted(1) }
  }

  it should "be executed twice when not testing - one once building scenario, and once at end " in {
    checkAssertionExecuted(2)
  }

  "Assertions" should "cause the build to fail with an AssertionException if they return false " in {

    scenario("A")
    update(_.expected(result("X")))
    assertion { false }
    evaluating { build } should produce[AssertionException]
    //TODO Check Assertion exception message
  }

}

abstract class EngineAssertion1Test[P, R] extends EngineAssertionTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class EngineAssertion2Test[P1, P2, R] extends EngineAssertionTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class EngineAssertion3Test[P1, P2, P3, R] extends EngineAssertionTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R,R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineAssertionStringStringTest extends EngineAssertion1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class EngineAssertionStringStringStringTest extends EngineAssertion2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class EngineAssertionStringStringStringStringTest extends EngineAssertion3Test[String, String, String, String] with StringStringStringStringTest
