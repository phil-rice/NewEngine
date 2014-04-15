package org.cddcore.engine

import org.junit.runner.RunWith
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner

abstract class EngineConflictTest[Params, BFn, R, RFn, B <: Builder[R, RFn, B], E <: Engine[Params, BFn, R, RFn]] extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, B, E] {
  implicit def toSome[X](x: X) = Some(x)
  implicit def toDecisionTreeDecisionTree[Params, BFn, R, RFn](x: Engine[Params, BFn, R, RFn]) = x.asInstanceOf[DecisionTree[Params, BFn, R, RFn]]

  protected def expectedMessageForComesToDifferentConclusionWhenThereIsADecisionNode: String
  protected def expectedMessageForNoBecauseWhenThereIsANoneDefaultConclusionAndNoBecauseNode: String
  protected def expectedMessageFoBecauseNotAdequate: String

  builderName should "throw ScenarioConflictingWithDefaultAndNoBecauseException if comes to different conclusion when there is decision node" in {
    scenario("A"); because("A"); expected("X")
    scenario("B"); code("Z"); expected("Z")
    val e = evaluating { build } should produce[ScenarioConflictingWithDefaultAndNoBecauseException]
    assertEquals(expectedMessageForComesToDifferentConclusionWhenThereIsADecisionNode, e.getMessage())
  }

  it should "throw ScenarioConflictingWithoutBecauseException if there is a non default conclusion and no because clause in the scenario" in {
    scenario("A"); because("A"); expected("X")
    scenario("AB"); expected("Y")
    val e = evaluating { build } should produce[ScenarioConflictingWithoutBecauseException]
    assertEquals(expectedMessageForNoBecauseWhenThereIsANoneDefaultConclusionAndNoBecauseNode, e.getMessage())
  }

  it should "throw ScenarioConflictAndBecauseNotAdequateException if the scenario being added has a different tconclusion and the because isn't good enough to differentiate it from the other scenarios in the conclusion with just root" in {
    scenario("AB"); expected("X")
    scenario("AC"); expected("X")
    scenario("BC"); expected("Y"); because("B")
    val e = evaluating { build } should produce[ScenarioConflictAndBecauseNotAdequateException]
    assertEquals(expectedMessageFoBecauseNotAdequate, e.getMessage())
  }
}

abstract class EngineConflict1Test[P, R] extends EngineConflictTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R], Engine1[P, R]] with Builder1Test[P, R]
abstract class EngineConflict2Test[P1, P2, R] extends EngineConflictTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R], Engine2[P1, P2, R]] with Builder2Test[P1, P2, R]
abstract class EngineConflict3Test[P1, P2, P3, R] extends EngineConflictTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R], Engine3[P1, P2, P3, R]] with Builder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineConflictStringStringTest extends EngineConflict1Test[String, String] with StringStringTest {
  protected val expectedMessageForComesToDifferentConclusionWhenThereIsADecisionNode = "\n" +
    "Lens(rootL.toDecisionL.noL)\n" +
    "Actual Result:\n" +
    "Left(org.cddcore.engine.UndecidedException: )\n" +
    "Expected\n" +
    "Right(Result(Z))\n" +
    "Scenario:\n" +
    "Scenario(B,None,None,None,Some(CodeHolder((p)=>resultZ)),None,Some(Right(Result(Z))),Set(),List(),List())\n" +
    "Parameters:\n" +
    "B"
  val expectedMessageForNoBecauseWhenThereIsANoneDefaultConclusionAndNoBecauseNode = "\n" +
    "Came to wrong conclusion: Right(Result(Y))\n" +
    "Instead of Right(Result(X))\n" +
    "Lens(rootL.toDecisionL.yesL)\n" +
    "Existing: A\n" +
    "Being Added: AB\n" +
    "Detailed existing:\n" +
    "Scenario(A,None,None,Some(CodeHolder(becauseA)),None,None,Some(Right(Result(X))),Set(),List(),List())\n" +
    "Detailed of being Added:\n" +
    "Scenario(AB,None,None,None,None,None,Some(Right(Result(Y))),Set(),List(),List())"
  val expectedMessageFoBecauseNotAdequate = "\n" +
    "The because is valid in these other scenarios, which the engine thinks are similar.\n" +
    "The Engine doesn't have enough information to decide what to do\n" +
    "To resolve this you could improve the because clause in this scenario to differentiate it from the listed scenarios\n" +
    "If you understand how the engine is constructed, you could change scenario priorities, but often it is better to refine the because clause\n" +
    "The because clause is becauseB\n" +
    "Came to wrong conclusion: Right(Result(X))\n" +
    "Instead of Right(Result(Y))\n" +
    "Lens(rootL)\n" +
    "Existing: AB\n" +
    "Being Added: BC\n" +
    "Detailed existing:\n" +
    "Scenario(AB,None,None,None,None,None,Some(Right(Result(X))),Set(),List(),List())\n" +
    "Detailed of being Added:\n" +
    "Scenario(BC,None,None,Some(CodeHolder(becauseB)),None,None,Some(Right(Result(Y))),Set(),List(),List())"

}

@RunWith(classOf[JUnitRunner])
class EngineConflictStringStringStringTest extends EngineConflict2Test[String, String, String] with StringStringStringTest {
  protected val expectedMessageForComesToDifferentConclusionWhenThereIsADecisionNode = "\n" +
    "Lens(rootL.toDecisionL.noL)\n" +
    "Actual Result:\n" +
    "Left(org.cddcore.engine.UndecidedException: )\n" +
    "Expected\n" +
    "Right(Z)\n" +
    "Scenario:\n" +
    "Scenario((B,B),None,None,None,Some(CodeHolder((p1,p2)=>resultZ)),None,Some(Right(Z)),Set(),List(),List())\n" +
    "Parameters:\n" +
    "(B,B)"
  val expectedMessageForNoBecauseWhenThereIsANoneDefaultConclusionAndNoBecauseNode = "\n" +
    "Came to wrong conclusion: Right(Y)\n" +
    "Instead of Right(X)\n" +
    "Lens(rootL.toDecisionL.yesL)\n" +
    "Existing: (A,A)\n" +
    "Being Added: (AB,AB)\n" +
    "Detailed existing:\n" +
    "Scenario((A,A),None,None,Some(CodeHolder(becauseA)),None,None,Some(Right(X)),Set(),List(),List())\n" +
    "Detailed of being Added:\n" +
    "Scenario((AB,AB),None,None,None,None,None,Some(Right(Y)),Set(),List(),List())"
  val expectedMessageFoBecauseNotAdequate = "\n" +
    "The because is valid in these other scenarios, which the engine thinks are similar.\n" +
    "The Engine doesn't have enough information to decide what to do\n" +
    "To resolve this you could improve the because clause in this scenario to differentiate it from the listed scenarios\n" +
    "If you understand how the engine is constructed, you could change scenario priorities, but often it is better to refine the because clause\n" +
    "The because clause is becauseB\n" +
    "Came to wrong conclusion: Right(X)\n" +
    "Instead of Right(Y)\n" +
    "Lens(rootL)\n" +
    "Existing: (AB,AB)\n" +
    "Being Added: (BC,BC)\n" +
    "Detailed existing:\n" +
    "Scenario((AB,AB),None,None,None,None,None,Some(Right(X)),Set(),List(),List())\n" +
    "Detailed of being Added:\n" +
    "Scenario((BC,BC),None,None,Some(CodeHolder(becauseB)),None,None,Some(Right(Y)),Set(),List(),List())"

}

@RunWith(classOf[JUnitRunner])
class EngineConflictStringStringStringStringTest extends EngineConflict3Test[String, String, String, String] with StringStringStringStringTest {
  protected val expectedMessageForComesToDifferentConclusionWhenThereIsADecisionNode = "\n" +
    "Lens(rootL.toDecisionL.noL)\n" +
    "Actual Result:\n" +
    "Left(org.cddcore.engine.UndecidedException: )\n" +
    "Expected\n" +
    "Right(Z)\n" +
    "Scenario:\n" +
    "Scenario((B,B,B),None,None,None,Some(CodeHolder((p1: P1, p2: P2, p3: P3) => Builder3Test.this.result(Z))),None,Some(Right(Z)),Set(),List(),List())\n" +
    "Parameters:\n" +
    "(B,B,B)"
  val expectedMessageForNoBecauseWhenThereIsANoneDefaultConclusionAndNoBecauseNode = "\n" +
    "Came to wrong conclusion: Right(Y)\n" +
    "Instead of Right(X)\n" +
    "Lens(rootL.toDecisionL.yesL)\n" +
    "Existing: (A,A,A)\n" +
    "Being Added: (AB,AB,AB)\n" +
    "Detailed existing:\n" +
    "Scenario((A,A,A),None,None,Some(CodeHolder(becauseA)),None,None,Some(Right(X)),Set(),List(),List())\n" +
    "Detailed of being Added:\n" +
    "Scenario((AB,AB,AB),None,None,None,None,None,Some(Right(Y)),Set(),List(),List())"
  val expectedMessageFoBecauseNotAdequate = "\n" +
    "The because is valid in these other scenarios, which the engine thinks are similar.\n" +
    "The Engine doesn't have enough information to decide what to do\n" +
    "To resolve this you could improve the because clause in this scenario to differentiate it from the listed scenarios\n" +
    "If you understand how the engine is constructed, you could change scenario priorities, but often it is better to refine the because clause\n" +
    "The because clause is becauseB\n" +
    "Came to wrong conclusion: Right(X)\n" +
    "Instead of Right(Y)\n" +
    "Lens(rootL)\n" +
    "Existing: (AB,AB,AB)\n" +
    "Being Added: (BC,BC,BC)\n" +
    "Detailed existing:\n" +
    "Scenario((AB,AB,AB),None,None,None,None,None,Some(Right(X)),Set(),List(),List())\n" +
    "Detailed of being Added:\n" +
    "Scenario((BC,BC,BC),None,None,Some(CodeHolder(becauseB)),None,None,Some(Right(Y)),Set(),List(),List())"

}
