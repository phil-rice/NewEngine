package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions

abstract class EngineFoldingTest[Params, BFn, R, RFn, FullR, B <: Builder[Params, BFn, R, RFn, FullR, B, E], E <: Engine[Params, BFn, R, RFn]]
  extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, FullR, B, E] with FoldingBuilderTest[R, FullR] {
  implicit def toSome[X](x: X) = Some(x)
  implicit def toFoldingEngine(e: Engine[Params, BFn, R, RFn]) = e.asInstanceOf[FoldingEngine[Params, BFn, R, RFn, FullR]]
  type FD = FoldingEngineDescription[R, RFn, FullR]
  type ED = EngineDescription[R, RFn]
  implicit def toFullR(seed: String): FullR
  implicit def toR(seed: String): R = result(seed)

  def compareFoldingEngineDescriptions(left: FD, nodes: List[BuilderNode[R, RFn]]) = {
    assertEquals(nodes.size, 1)
    val right = nodes.head.asInstanceOf[FD]
    val withDifferentFoldingStuff = left.copy(foldingFn = right.foldingFn, initialValue = right.initialValue)
    assertEquals(withDifferentFoldingStuff, right)
  }

  "A folding builder" should "have a folding engine description with engine description when created" in {
    val b = currentBuilder
    val empty: FD = FoldingEngineDescription(initialValue = () => initialValue, foldingFn = foldingFn)
    compareFoldingEngineDescriptions(empty, currentBuilder.nodes)

  }
  it should "add the child engines under the main engine" in {
    update(_.childEngine("ce1").useCase("uc11").useCase("uc12"))
    update(_.childEngine("ce2").useCase("uc21").useCase("uc22"))
    update(_.childEngine("ce3").useCase("uc31").useCase("uc32"))

    val ce1 = EngineDescription[R, RFn](title = "ce1", nodes = List(UseCase(title = "uc12"), UseCase(title = "uc11")))
    val ce2 = EngineDescription[R, RFn](title = "ce2", nodes = List(UseCase(title = "uc22"), UseCase(title = "uc21")))
    val ce3 = EngineDescription[R, RFn](title = "ce3", nodes = List(UseCase(title = "uc32"), UseCase(title = "uc31")))
    val fe = FoldingEngineDescription[R, RFn, FullR](nodes = List(ce3, ce2, ce1), foldingFn = foldingFn, initialValue = () => initialValue)
    val actualFr = currentBuilder.nodes.head.asInstanceOf[FoldingEngineDescription[R, RFn, FullR]]
    val actualCe3 = actualFr.nodes.head.asInstanceOf[EngineDescription[R, RFn]]
    val actualUC32 = actualCe3.nodes.head
    val withDifferentFoldingStuff = actualFr.copy(foldingFn = fe.foldingFn, initialValue = fe.initialValue)
    assertEquals(UseCase[R, RFn](title = "uc32"), actualUC32)
    assertEquals(ce3, actualCe3)
    assertEquals(fe.nodes, withDifferentFoldingStuff.nodes)
    assertEquals(fe, withDifferentFoldingStuff)
  }

  it should "create a folding engine with child engines" in {
    update(_.childEngine("ce1"))
    scenario("A")
    expected("X")
    update(_.childEngine("ce2"))
    scenario("B")
    expected("Y")
    val e = build

    val expectedValue = List[R]("X", "Y").foldLeft(initialValue)(foldingFn)
    assertEquals(expectedValue, e.applyParams(params("a")))
  }

  it should "throw an Exception if no child engines are specified" in {
    evaluating { build } should produce[CannotHaveFoldingEngineWithoutChildEnginesException]
  }

  it should "have a buildExceptions that is the aggregate of the child engine's when executed in test mode" in {
    val e0 = new RuntimeException("e0");
    val e1 = new RuntimeException("e1");
    val e = Engine.test {
      update(_.childEngine("ce1"))
      scenario("A")
      expected("Y")
      becauseException(e0)
      update(_.childEngine("ce2"))
      scenario("B")
      expected("Y")
      becauseException(e1)

      build
    }

    val eMap = e.buildExceptions
    val sa = s("A", expected = "Y")
    val sb = s("B", expected = "Y")
    val mapToListOfClasses = eMap.mapValues((listE) => listE.map(_.getClass))
    val mapToListOfExceptions = eMap.mapValues((listE) => listE.map(_.getCause))
    val clazz = classOf[BecauseClauseScenarioException]
    assertEquals(Map(sa -> List(clazz), sb -> List(clazz)), mapToListOfClasses)
    assertEquals(Map(sa -> List(e0), sb -> List(e1)), mapToListOfExceptions)

  }
}

abstract class EngineFolding1Test[P, R, FullR]
  extends EngineFoldingTest[P, (P) => Boolean, R, (P) => R, FullR, Builder1[P, R, FullR], Engine1[P, R, FullR]]
  with FoldingBuilder1Test[P, R, FullR] {
  def initialiseAsFoldingEngine = update { (x) => Engine.folding[P, R, FullR](initialValue, foldingFn) }
}
abstract class EngineFolding2Test[P1, P2, R, FullR] extends EngineFoldingTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, FullR, Builder2[P1, P2, R, FullR], Engine2[P1, P2, R, FullR]]
  with Builder2Test[P1, P2, R, FullR]
  with FoldingBuilder2Test[P1, P2, R, FullR] {
  def initialiseAsFoldingEngine = update { (x) => Engine.folding[P1, P2, R, FullR](initialValue, foldingFn) }

}
abstract class EngineFolding3Test[P1, P2, P3, R, FullR] extends EngineFoldingTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, FullR, Builder3[P1, P2, P3, R, FullR], Engine3[P1, P2, P3, R, FullR]]
  with Builder3Test[P1, P2, P3, R, FullR]
  with FoldingBuilder3Test[P1, P2, P3, R, FullR] {
  def initialiseAsFoldingEngine = update { (x) => Engine.folding[P1, P2, P3, R, FullR](initialValue, foldingFn) }
}

trait StringToStringFoldingTest {
  def foldingFn = (x: String, y: String) => { x + y }
  def initialValue: String = "_"
  implicit def toFullR(x: String) = x
}

@RunWith(classOf[JUnitRunner])
class EngineFoldingStringStringTest extends EngineFolding1Test[String, String, String] with StringStringTest with StringToStringFoldingTest

@RunWith(classOf[JUnitRunner])
class EngineFoldingStringStringStringTest extends EngineFolding2Test[String, String, String, String] with StringStringStringTest with StringToStringFoldingTest

@RunWith(classOf[JUnitRunner])
class EngineFoldingStringStringStringStringTest extends EngineFolding3Test[String, String, String, String, String] with StringStringStringStringTest with StringToStringFoldingTest
