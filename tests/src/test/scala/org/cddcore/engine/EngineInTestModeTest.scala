package org.cddcore.engine

import org.junit.runner.RunWith
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner

abstract class EngineInTestModeTest[Params, BFn, R, RFn, B <: Builder[R, RFn, B], E <: Engine[Params, BFn, R, RFn]] extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, B, E] with ConflictMessages {
  implicit def toSome[X](x: X) = Some(x)

  def checkExceptions(doIt: => Unit, scenarioClassMessage: (Scenario[Params, BFn, R, RFn], List[(Class[_], String)])*) = {
    val messages = scenarioClassMessage.toList
    val e = Engine.test {
      doIt
      build
    }
    val exceptions = e.buildExceptions
    assertEquals(messages.size, exceptions.size)
    for ((s, list) <- messages) {
      val actual = exceptions(s)
      for ((e, (c, msg)) <- actual.zip(list)) {
        assertEquals(c, e.getClass)
        assertEquals(msg, e.getMessage())
      }
    }
  }

  builderName should "store ScenarioConflictingWithDefaultAndNoBecauseException if comes to different conclusion when there is decision node in test mode" in {

    checkExceptions(
      {
        scenario("A"); because("A"); expected("X")
        scenario("B"); code("Z"); expected("Z")
      },
      (s("B", code = resultCodeHolder("Z"), expected = "Z"), List((classOf[ScenarioConflictingWithDefaultAndNoBecauseException], expectedMessageForComesToDifferentConclusionWhenThereIsADecisionNode))))
  }

  it should "store ScenarioConflictingWithoutBecauseException if there is a non default conclusion and no because clause in the scenario in test mode" in {
    scenario("A"); because("A"); expected("X")
    scenario("AB"); expected("Y")
    val e = evaluating { build } should produce[ScenarioConflictingWithoutBecauseException]
    assertEquals(expectedMessageForNoBecauseWhenThereIsANoneDefaultConclusionAndNoBecauseNode, e.getMessage())
  }

  it should "store ScenarioConflictAndBecauseNotAdequateException if the scenario being added has a different tconclusion and the because isn't good enough to differentiate it from the other scenarios in the conclusion with just root in test mode" in {
    scenario("AB"); expected("X")
    scenario("AC"); expected("X")
    scenario("BC"); expected("Y"); because("B")
    val e = evaluating { build } should produce[ScenarioConflictAndBecauseNotAdequateException]
    assertEquals(expectedMessageFoBecauseNotAdequate, e.getMessage())
  }
}

abstract class EngineInTestMode1Test[P, R] extends EngineInTestModeTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R], Engine1[P, R]] with Builder1Test[P, R]
abstract class EngineInTestMode2Test[P1, P2, R] extends EngineInTestModeTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R], Engine2[P1, P2, R]] with Builder2Test[P1, P2, R]
abstract class EngineInTestMode3Test[P1, P2, P3, R] extends EngineInTestModeTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R], Engine3[P1, P2, P3, R]] with Builder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineInTestModeStringStringTest extends EngineInTestMode1Test[String, String] with StringStringTest with ConflictMessages1

@RunWith(classOf[JUnitRunner])
class EngineInTestModeStringStringStringTest extends EngineInTestMode2Test[String, String, String] with StringStringStringTest with ConflictMessages2

@RunWith(classOf[JUnitRunner])
class EngineInTestModeStringStringStringStringTest extends EngineInTestMode3Test[String, String, String, String] with StringStringStringStringTest with ConflictMessages3

