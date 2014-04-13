package org.cddcore.engine

import org.junit.runner.RunWith
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner

abstract class EngineNodeConstructionTest[Params, BFn, R, RFn, B <: Builder[R, RFn, B], E <: Engine[Params, BFn, R, RFn]] extends BuilderTest[Params, BFn, R, RFn, B, E] {
  implicit def toSome[X](x: X) = Some(x)

  builderName should "allow an empty engine to be made" in {
    val b = currentBuilder
    assertEquals(List(EngineDescription[R, RFn]()), currentBuilder.nodes)
  }

  it should "allow the engine descriptions  to be set" in {
    update((b) => b.title("EngineTitle").description("EngineDescription").priority(1).expected(result(0)))
    code(4)
    assertEquals(List(EngineDescription[R, RFn](title = "EngineTitle", description = "EngineDescription", priority = 1, expected=Right(result(0)))), currentBuilder.nodes)
  }

  it should "allow a use case to be added" in {
    update((b) => b.title("EngineTitle").useCase("useCase1").expected(result(0)))
    assertEquals(List(EngineDescription[R, RFn](
      title = "EngineTitle",
      nodes = List(UseCase(title = "useCase1", expected = Right(result(0)))))), currentBuilder.nodes)
  }
  it should "allow a seconduse case to be added" in {
    update((b) => b.title("EngineTitle").useCase("useCase1").useCase("useCase2"))
    val ed = currentBuilder.nodes.head.asInstanceOf[EngineDescription[R, RFn]]
    val uc = ed.nodes.head
    assertEquals(List(EngineDescription[R, RFn](
      title = "EngineTitle",
      nodes = List(UseCase(title = "useCase2"), UseCase(title = "useCase1")))), currentBuilder.nodes)
  }
}

abstract class EngineNodeConstruction1Test[P, R] extends EngineNodeConstructionTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R], Engine1[P, R]] with Builder1Test[P, R]
abstract class EngineNodeConstruction2Test[P1, P2, R] extends EngineNodeConstructionTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R], Engine2[P1, P2, R]] with Builder2Test[P1, P2, R]
abstract class EngineNodeConstruction3Test[P1, P2, P3, R] extends EngineNodeConstructionTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R], Engine3[P1, P2, P3, R]] with Builder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineNodeConstructionStringIntTest extends EngineNodeConstruction1Test[String, Int] with StringIntTest

@RunWith(classOf[JUnitRunner])
class EngineNodeConstructionStringStringTest extends EngineNodeConstruction1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class EngineNodeConstructionStringStringStringTest extends EngineNodeConstruction2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class EngineNodeConstructionStringStringStringStringTest extends EngineNodeConstruction3Test[String, String, String, String] with StringStringStringStringTest

object BuildingTest {
  def main(args: Array[String]) {
  }
}