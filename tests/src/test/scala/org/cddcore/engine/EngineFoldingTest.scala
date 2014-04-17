package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions

abstract class EngineFoldingTest[Params, BFn, R, RFn, FullR, B <: Builder[Params, BFn, R, RFn, FullR, B, E], E <: Engine[Params, BFn, R, RFn]]
  extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, FullR, B, E] {
  implicit def toSome[X](x: X) = Some(x)
  def initialiseAsFoldingEngine
  def foldingFn: (FullR, R) => FullR
  def initialValue: FullR

//  "An builder that hasn't been set up as a folding builder" should "throw CannotHaveChildEnginesWithoutFolderException" in {
//    evaluating { update(_.childEngine("some title")) } should produce[CannotHaveChildEnginesWithoutFolderException]
//  }
//
//  "An builder with child engines" should "add the child engines under the main engine" in {
//    initialiseAsFoldingEngine
//    update(_.childEngine("ce1").useCase("uc11").useCase("uc12"))
//    update(_.childEngine("ce2").useCase("uc21").useCase("uc22"))
//    update(_.childEngine("ce3").useCase("uc31").useCase("uc32"))
//
//    val ce1 = EngineDescription[R, RFn](title = "ce1", nodes = List(UseCase(title = "uc12"), UseCase(title = "uc11")))
//    val ce2 = EngineDescription[R, RFn](title = "ce2", nodes = List(UseCase(title = "uc22"), UseCase(title = "uc21")))
//    val ce3 = EngineDescription[R, RFn](title = "ce3", nodes = List(UseCase(title = "uc32"), UseCase(title = "uc31")))
//    val fe = FoldingEngineDescription[R, RFn, FullR](nodes = List(ce3, ce2, ce1), foldingFn = foldingFn, initialValue = () => initialValue)
//    val actualFr = currentBuilder.nodes.head.asInstanceOf[FoldingEngineDescription[R, RFn, FullR]]
//    val actualCe3 = actualFr.nodes.head.asInstanceOf[EngineDescription[R, RFn]]
//    val actualUC32 = actualCe3.nodes.head
//    val withDifferentFoldingStuff = actualFr.copy(foldingFn = fe.foldingFn, initialValue = fe.initialValue)
//    assertEquals(UseCase[R, RFn](title = "uc32"), actualUC32)
//    assertEquals(ce3, actualCe3)
//    assertEquals(fe.nodes, withDifferentFoldingStuff.nodes)
//    assertEquals(fe, withDifferentFoldingStuff)
//  }
//
//  it should "create a folding engine with child engines" in {
//    initialiseAsFoldingEngine
//    update(_.childEngine("ce1"))
//    scenario("A")
//    expected("A")
//    update(_.childEngine("ce2"))
//    scenario("B")
//    expected("B")
//    val e = build
//
//  }

  //  "An engine with a child engine" should "allow each child engine to come to correct conclusion" in {
  //    val childEngines = Engine[Int, String]().
  //      childEngine("ce0").scenario(0).expected("zero").
  //      childEngine("ce1").scenario(1).expected("one").
  //      builderData.childrenModifiedForBuild.collect { case e: ChildEngine[_] => e }
  //
  //    assertEquals("zero", childEngines(0).applyParams(List(0)))
  //    assertEquals("zero", childEngines(0).applyParams(List(1)))
  //
  //    assertEquals("one", childEngines(1).applyParams(List(0)))
  //    assertEquals("one", childEngines(1).applyParams(List(1)))
  //  }
  //
  //  it should "have a toString method composed of the title strings of it's children" in {
  //    val eBase = Engine.folding[Int, String, String]((acc, r) => acc, "").
  //      childEngine("ce0").scenario(0).expected("zero").
  //      childEngine("ce1").scenario(1).expected("one").
  //      build
  //    val e01 = Engine.folding[Int, String, String]((acc, r) => acc, "").
  //      childEngine("ce0").scenario(0).expected("zero").
  //      childEngine("ce1").priority(1).scenario(1).expected("one").
  //      build
  //    val e10 = Engine.folding[Int, String, String]((acc, r) => acc, "").
  //      childEngine("ce0").priority(1).scenario(0).expected("zero").
  //      childEngine("ce1").scenario(1).expected("one").
  //      build
  //    assertEquals("EngineWithChildren(ce0,ce1)", e01.toString)
  //    assertEquals("EngineWithChildren(ce1,ce0)", e10.toString)
  //    assertEquals("EngineWithChildren(ce0,ce1)", eBase.toString)
  //  }
  //  it should "use the fold function to produce the correct result" in {
  //    val b = Engine.folding[Int, String, String](_ + _, { "Init" }).
  //      childEngine("ce0").scenario(0).expected("Zero").
  //      childEngine("ce1").scenario(1).expected("One")
  //    val e = b.build
  //    val result = e(0)
  //    assertEquals("InitZeroOne", e(0))
  //    assertEquals("InitZeroOne", e(123))
  //  }
  //
  //  it should "throw a exception if a fold function has been specified and there are no child engines" in {
  //    evaluating {
  //      Engine.foldList[Int, String].build
  //    } should produce[CannotHaveFolderWithoutChildEnginesException]
  //    evaluating { Engine.foldList[Int, Int, String].build } should produce[CannotHaveFolderWithoutChildEnginesException]
  //    evaluating { Engine.foldList[Int, Int, Int, String].build } should produce[CannotHaveFolderWithoutChildEnginesException]
  //
  //  }
  //
  //  it should "have a ScenarioExceptionMap that is the aggregate of the child engine's" in {
  //    import EngineWithScenarioExceptionMap._
  //    val e0 = new RuntimeException("e0");
  //    val e1 = new RuntimeException("e1");
  //    val e = Engine.test {
  //      Engine.foldList[Int, String].
  //        childEngine("ce1").scenario(0).expected("x").because((x: Int) => throw e0).
  //        childEngine("ce2").scenario(1).expected("x").code((x: Int) => throw new RuntimeException(e1)).
  //        build
  //    }
  //    val scenarios = e.all(classOf[Test])
  //    val s0 = scenarios(0)
  //    val s1 = scenarios(1)
  //    //    assertEquals(e0, e.scenarioExceptionMap(s0).asInstanceOf[BecauseClauseException].getCause())
  //    //    assertEquals(e1, e.scenarioExceptionMap(s1))
  //  }

}
//
//abstract class EngineFolding1Test[P, R, FullR] extends EngineFoldingTest[P, (P) => Boolean, R, (P) => R, FullR, Builder1[P, R, FullR], Engine1[P, R, R]] with Builder1Test[P, R, FullR] {
//  def initialiseAsFoldingEngine = update { (x) => Engine.folding[P, R, FullR](initialValue, foldingFn) }
//}
//abstract class EngineFolding2Test[P1, P2, R, FullR] extends EngineFoldingTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, FullR, Builder2[P1, P2, R, FullR], Engine2[P1, P2, R, R]] with Builder2Test[P1, P2, R, FullR] {
//  def initialiseAsFoldingEngine = update { (x) => Engine.folding[P1, P2, R, FullR](initialValue, foldingFn) }
//
//}
//abstract class EngineFolding3Test[P1, P2, P3, R, FullR] extends EngineFoldingTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, FullR, Builder3[P1, P2, P3, R, FullR], Engine3[P1, P2, P3, R,R]] with Builder3Test[P1, P2, P3, R, FullR] {
//  def initialiseAsFoldingEngine = update { (x) => Engine.folding[P1, P2, P3, R, FullR](initialValue, foldingFn) }
//}
//
//trait StringToStringFoldingTest {
//  def foldingFn = (x: String, y: String) => { x + y }
//  def initialValue: String = "_"
//}
//@RunWith(classOf[JUnitRunner])
//class EngineFoldingStringStringTest extends EngineFolding1Test[String, String, String] with StringStringTest with StringToStringFoldingTest
//
//@RunWith(classOf[JUnitRunner])
//class EngineFoldingStringStringStringTest extends EngineFolding2Test[String, String, String, String] with StringStringStringTest with StringToStringFoldingTest
//
//@RunWith(classOf[JUnitRunner])
//class EngineFoldingStringStringStringStringTest extends EngineFolding3Test[String, String, String, String, String] with StringStringStringStringTest with StringToStringFoldingTest
