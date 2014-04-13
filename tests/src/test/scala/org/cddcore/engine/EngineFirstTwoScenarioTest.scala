package org.cddcore.engine

import org.junit.runner.RunWith
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner

abstract class EngineFirstTwoScenarioTest[Params, BFn, R, RFn, B <: Builder[R, RFn, B], E <: Engine[Params, BFn, R, RFn]] extends BuilderTest[Params, BFn, R, RFn, B, E] {
  implicit def toDecisionTreeDecisionTree[Params, BFn, R, RFn](x: Engine[Params, BFn, R, RFn]) = x.asInstanceOf[DecisionTree[Params, BFn, R, RFn]]
  implicit def toSome[X](x: X) = Some(x)
  "An empty engine" should "allow the first use not to have a because, and become the default value" in {
    scenario(0)
    update(_.expected(result(1)))
    val e = build
    val s0 = s(0).copyEngineNode(expected = Some(Right(result(1)))).asInstanceOf[Scenario[Params, BFn, R, RFn]]
    assertEquals(Conclusion(List(s0), s0.actualCode(e.expectedToCode)), e.root)

    assertEquals(result(1), e.evaluate(params(0)))
    assertEquals(result(1), e.evaluate(params(1)))
  }

  it should " allow the first use not to have a because, and become the default value when we add a second scenario " in {
    scenario(1)
    update(_.expected(result(0)))
    scenario(2)
    update(_.expected(result(1)))
    because(1)
    val e = build

    val s1 = s(1).copyEngineNode(expected = Some(Right(result(0)))).asInstanceOf[Scenario[Params, BFn, R, RFn]]
    val s2 = s(2).copyEngineNode(expected = Some(Right(result(1)))).asInstanceOf[Scenario[Params, BFn, R, RFn]].copyScenario(because = becauseCodeHolder(1))
    val conc1 = Conclusion(List(s1), s1.actualCode(e.expectedToCode))
    val conc2 = Conclusion(List(s2), s2.actualCode(e.expectedToCode))
    val dec = Decision(List(becauseCodeHolder(1)), yes=conc2, no=conc1, scenarioThatCausedNode = s2)
    assertEquals(dec, e.root)
    
    assertEquals(result(0), e.evaluate(params(1)))
    assertEquals(result(1), e.evaluate(params(2)))
    assertEquals(result(0), e.evaluate(params(333)))
  }

//  it should "still throw an exception if a because clause is given by the first scenario when parameters don't match the because clause" in {
//    val e = Engine[Int, String]().scenario(1).expected("x").because((x: Int) => x == 1).build
//    assertEquals("x", e(1))
//    evaluating { e(2) } should produce[UndecidedException]
//  }
  //
  //  it should " allow the first use not to have a because, and become the default value when we add a second scenario in same used case" in {
  //    val e = org.cddcore.engine.Engine[Int, String]().
  //      useCase("").scenario(1).expected("x").
  //      scenario(2).expected("y").because((x: Int) => x == 2).
  //      build
  //  }
  //
  //  it should "throw a DuplicateScenarioException is the same scenario is added" in {
  //    val b = org.cddcore.engine.Engine[Int, String]().
  //      useCase("").scenario(1).expected("x").
  //      scenario(1).expected("x")
  //    evaluating { b.build } should produce[DuplicateScenarioException]
  //  }
  //
  //  it should "produce a simple if then with two scenarios" in {
  //    val b = builderWithDefault.scenario("B").because("B").expected("X");
  //    val e = b.build
  //    assertEquals(2, e.tests.size)
  //    assertEquals(defaultScenario, e.tests(0))
  //    val bScenario = e.tests(1)
  //    assertEngineMatches(e, Right(EngineNode(because = List("B"), inputs = List("B"), yes = Left(CodeAndScenarios("X", List(bScenario))), no = Left(CodeAndScenarios("Z", List(defaultScenario))), scenarioThatCausedNode = bScenario)))
  //  }
  //
  //  it should "Throw ScenarioConflictingWithDefaultException if second scenario is assertion and comes to wrong result" in {
  //    val b = builderWithDefault.scenario("A").expected("X");
  //    evaluating {
  //      b.build
  //    } should produce[ScenarioConflictingWithDefaultException]
  //  }
  //
  //  it should "Add scenario to root if adding assertion" in {
  //    val b = builderWithDefault.scenario("B").expected("Z")
  //    val e1 = builderWithDefault.build
  //    val e2 = b.build
  //    val bScenario = e2.tests(1)
  //    assertEngineMatches(e1, Left(CodeAndScenarios("Z", List(defaultScenario))))
  //    assertEngineMatches(e2, Left(CodeAndScenarios("Z", List(bScenario, defaultScenario))))
  //  }
  //
  //  it should "Add scenario to root if adding with same conclusion, different reason" in {
  //    val b = builderWithDefault.scenario("B").because("B").expected("Z")
  //    val e1 = builderWithDefault.build
  //    val e2 = b.build
  //    val bScenario = e2.tests(1)
  //    assertEngineMatches(e1, Left(CodeAndScenarios("Z", List(defaultScenario))))
  //    assertEngineMatches(e2, Left(CodeAndScenarios("Z", List(bScenario, defaultScenario))))
  //  }
  //
  //  it should "Throw NoExpectedException if scenario doesnt have expected" in {
  //    evaluating { builderWithScenario.code("Z").build } should produce[NoExpectedException]
  //    //    evaluating { builderWithDefault.because("A").code("X").build } should produce[NoExpectedException]
  //  }
  //
  //  it should "Throw ScenarioBecauseException if because is not true in scenario" in {
  //    val b = builderWithDefault.scenario("B").because("X")
  //    //    b.build
  //    evaluating { b.build } should produce[ScenarioBecauseException]
  //  }

}

abstract class EngineFirstTwoScenario1Test[P, R] extends EngineFirstTwoScenarioTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R], Engine1[P, R]] with Builder1Test[P, R]
abstract class EngineFirstTwoScenario2Test[P1, P2, R] extends EngineFirstTwoScenarioTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R], Engine2[P1, P2, R]] with Builder2Test[P1, P2, R]
abstract class EngineFirstTwoScenario3Test[P1, P2, P3, R] extends EngineFirstTwoScenarioTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R], Engine3[P1, P2, P3, R]] with Builder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineFirstTwoScenarioStringIntTest extends EngineFirstTwoScenario1Test[String, Int] with StringIntTest

@RunWith(classOf[JUnitRunner])
class EngineFirstTwoScenarioStringStringTest extends EngineFirstTwoScenario1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class EngineFirstTwoScenarioStringStringStringTest extends EngineFirstTwoScenario2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class EngineFirstTwoScenarioStringStringStringStringTest extends EngineFirstTwoScenario3Test[String, String, String, String] with StringStringStringStringTest

