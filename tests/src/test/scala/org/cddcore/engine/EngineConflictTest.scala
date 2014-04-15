package org.cddcore.engine

import org.junit.runner.RunWith
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner

abstract class EngineConflictTest[Params, BFn, R, RFn, B <: Builder[R, RFn, B], E <: Engine[Params, BFn, R, RFn]] extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, B, E] {
  implicit def toSome[X](x: X) = Some(x)
  implicit def toDecisionTreeDecisionTree[Params, BFn, R, RFn](x: Engine[Params, BFn, R, RFn]) = x.asInstanceOf[DecisionTree[Params, BFn, R, RFn]]

  protected def expectedMessageForComesToDifferentConclusionWhenThereIsADecisionNode: String
  protected def expectedMessageForNoBecauseWhenThereIsANoneDefaultConclusionAndNoBecauseNode: String

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
}

abstract class EngineConflict1Test[P, R] extends EngineConflictTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R], Engine1[P, R]] with Builder1Test[P, R]
abstract class EngineConflict2Test[P1, P2, R] extends EngineConflictTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R], Engine2[P1, P2, R]] with Builder2Test[P1, P2, R]
abstract class EngineConflict3Test[P1, P2, P3, R] extends EngineConflictTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R], Engine3[P1, P2, P3, R]] with Builder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineConflictStringStringTest extends EngineConflict1Test[String, String] with StringStringTest {
  protected val expectedMessageForComesToDifferentConclusionWhenThereIsADecisionNode = "\n" +
    "Lens(rootL.toDecisionL.noL.toConclusionL)\n" +
    "Actual Result:\n" +
    "Left(org.cddcore.engine.UndecidedException: )\n" +
    "Expected\n" +
    "Right(Result(Z))\n" +
    "Scenario:\n" +
    "Scenario(B,None,None,None,Some(CodeHolder((p)=>resultZ)),None,Some(Right(Result(Z))),Set(),List())\n" +
    "Parameters:\n" +
    "B"
  val expectedMessageForNoBecauseWhenThereIsANoneDefaultConclusionAndNoBecauseNode = "\n" +
    "Came to wrong conclusion: Right(Result(Y))\n" +
    "Instead of Right(Result(X))\n" +
    "Lens(rootL.toDecisionL.yesL.toConclusionL)\n" +
    "Existing: A\n" +
    "Being Added: AB\n" +
    "Detailed existing:\n" +
    "Scenario(A,None,None,Some(CodeHolder(becauseA)),None,None,Some(Right(Result(X))),Set(),List())\n" +
    "Detailed of being Added:\n" +
    "Scenario(AB,None,None,None,None,None,Some(Right(Result(Y))),Set(),List())"
}

@RunWith(classOf[JUnitRunner])
class EngineConflictStringStringStringTest extends EngineConflict2Test[String, String, String] with StringStringStringTest {
  protected val expectedMessageForComesToDifferentConclusionWhenThereIsADecisionNode = "\n" +
    "Lens(rootL.toDecisionL.noL.toConclusionL)\n" +
    "Actual Result:\n" +
    "Left(org.cddcore.engine.UndecidedException: )\n" +
    "Expected\n" +
    "Right(Z)\n" +
    "Scenario:\n" +
    "Scenario((B,B),None,None,None,Some(CodeHolder((p1,p2)=>resultZ)),None,Some(Right(Z)),Set(),List())\n" +
    "Parameters:\n" +
    "(B,B)"
  val expectedMessageForNoBecauseWhenThereIsANoneDefaultConclusionAndNoBecauseNode = "\n" +
    "Came to wrong conclusion: Right(Y)\n" +
    "Instead of Right(X)\n" +
    "Lens(rootL.toDecisionL.yesL.toConclusionL)\n" +
    "Existing: (A,A)\n" +
    "Being Added: (AB,AB)\n" +
    "Detailed existing:\n" +
    "Scenario((A,A),None,None,Some(CodeHolder(becauseA)),None,None,Some(Right(X)),Set(),List())\n" +
    "Detailed of being Added:\n" +
    "Scenario((AB,AB),None,None,None,None,None,Some(Right(Y)),Set(),List())"

}

@RunWith(classOf[JUnitRunner])
class EngineConflictStringStringStringStringTest extends EngineConflict3Test[String, String, String, String] with StringStringStringStringTest {
  protected val expectedMessageForComesToDifferentConclusionWhenThereIsADecisionNode = "\n" +
    "Lens(rootL.toDecisionL.noL.toConclusionL)\n" +
    "Actual Result:\n" +
    "Left(org.cddcore.engine.UndecidedException: )\n" +
    "Expected\n" +
    "Right(Z)\n" +
    "Scenario:\n" +
    "Scenario((B,B,B),None,None,None,Some(CodeHolder((p1: P1, p2: P2, p3: P3) => Builder3Test.this.result(Z))),None,Some(Right(Z)),Set(),List())\n" +
    "Parameters:\n" +
    "(B,B,B)"
  val expectedMessageForNoBecauseWhenThereIsANoneDefaultConclusionAndNoBecauseNode = "\n" +
    "Came to wrong conclusion: Right(Y)\n" +
    "Instead of Right(X)\n" +
    "Lens(rootL.toDecisionL.yesL.toConclusionL)\n" +
    "Existing: (A,A,A)\n" +
    "Being Added: (AB,AB,AB)\n" +
    "Detailed existing:\n" +
    "Scenario((A,A,A),None,None,Some(CodeHolder(becauseA)),None,None,Some(Right(X)),Set(),List())\n" +
    "Detailed of being Added:\n" +
    "Scenario((AB,AB,AB),None,None,None,None,None,Some(Right(Y)),Set(),List())"
}
