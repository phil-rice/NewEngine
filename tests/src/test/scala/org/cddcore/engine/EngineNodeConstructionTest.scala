package org.cddcore.engine

import org.junit.runner.RunWith
import scala.language.implicitConversions
import org.scalatest.junit.JUnitRunner

abstract class EngineNodeConstructionTest[Params, BFn, R, RFn, B <: Builder[R, RFn, B], E <: Engine[Params, BFn, R, RFn]] extends BuilderTest[Params, BFn, R, RFn, B, E] {
  implicit def toSome[X](x: X) = Some(x)
  val doc = Document()
  val ref1 = Reference("1", doc)
  val ref2 = Reference("2", doc)
  val ref3 = Reference("3", doc)
  val ref1None = Reference("1", None)
  val ref2None = Reference("2", None)
  val ref3None = Reference("3", None)

  builderName should "allow an empty engine to be made" in {
    val b = currentBuilder
    assertEquals(List(EngineDescription[R, RFn]()), currentBuilder.nodes)
  }

  it should "allow the engine descriptions  to be set" in {
    update((b) => b.title("EngineTitle").description("EngineDescription").priority(1).expected(result(0)))
    code(4)
    assertEquals(List(EngineDescription[R, RFn](title = "EngineTitle", description = "EngineDescription", code = resultCodeHolder(4), priority = 1, expected = Right(result(0)))),
      currentBuilder.nodes)
  }

  it should "allow a use case to be added" in {
    update((b) => b.title("EngineTitle").useCase("useCase1").expected(result(0)))
    assertEquals(List(EngineDescription[R, RFn](
      title = "EngineTitle",
      nodes = List(UseCase(title = "useCase1", expected = Right(result(0)))))), currentBuilder.nodes)
  }
  it should "allow a second use case to be added" in {
    update((b) => b.title("EngineTitle").useCase("useCase1").useCase("useCase2"))
    assertEquals(List(EngineDescription[R, RFn](title = "EngineTitle", nodes = List(UseCase(title = "useCase2"), UseCase(title = "useCase1")))), currentBuilder.nodes)
  }

  it should "allow a scenario to added under an engine" in {
    update((b) => b.title("EngineTitle"))
    scenario(0)
    assertEquals(List(EngineDescription[R, RFn](title = "EngineTitle", nodes = List(scenarioObject(params(0))))), currentBuilder.nodes)

  }
  it should "allow a scenario to added under a usecase" in {
    update((b) => b.title("EngineTitle").useCase("useCase1"))
    scenario(0)
    assertEquals(List(EngineDescription[R, RFn](title = "EngineTitle", nodes = List(UseCase(title = "useCase1", nodes = List(scenarioObject(params(0))))))), currentBuilder.nodes)
  }
  it should "allow a multiple scenarios to added under a usecase" in {
    update((b) => b.title("EngineTitle").useCase("useCase1"))
    scenario(0)
    scenario(1)
    scenario(2)
    assertEquals(List(EngineDescription[R, RFn](title = "EngineTitle", nodes = List(UseCase(title = "useCase1", nodes = List(
      scenarioObject(params(2)), scenarioObject(params(1)), scenarioObject(params(0))))))), currentBuilder.nodes)
  }

  it should "allow references to be added" in {

    update((b) => b.title("EngineTitle").reference("1", doc).useCase("useCase1").reference("2", doc))
    scenario(0)
    update((b) => b.reference("3", doc))
    assertEquals(List(
      EngineDescription[R, RFn](title = "EngineTitle", references = Set(ref1), nodes = List(
        UseCase(title = "useCase1", references = Set(ref2), nodes = List(
          scenarioObject(params(0)).copyRequirement(references = Set(ref3))))))), currentBuilder.nodes)
  }
  it should "allow references to be added when doc is not specified" in {

    update((b) => b.title("EngineTitle").reference("1").useCase("useCase1").reference("2"))
    scenario(0)
    update((b) => b.reference("3"))
    assertEquals(List(
      EngineDescription[R, RFn](title = "EngineTitle", references = Set(ref1None), nodes = List(
        UseCase(title = "useCase1", references = Set(ref2None), nodes = List(
          scenarioObject(params(0)).copyRequirement(references = Set(ref3None))))))), currentBuilder.nodes)
  }

  it should "allow multiple references to be added" in {
    update((b) => b.title("EngineTitle").reference("1", doc).reference("2", doc).reference("3", doc))
    assertEquals(List(EngineDescription[R, RFn](title = "EngineTitle", references = Set(ref1, ref2, ref3))), currentBuilder.nodes)
  }

  it should "allow code to be added" in {
    code(1)
    update(_.useCase("UC"))
    code(2)
    scenario(0)
    code(3)

    assertEquals(List(EngineDescription[R, RFn](code = resultCodeHolder(1), nodes = List(
      UseCase(title = "UC", code = resultCodeHolder(2), nodes = List(
        scenarioObject(params(0)).copyEngineNode(code = resultCodeHolder(3))))))),
      currentBuilder.nodes)
  }

  it should "allow because to be added" in {
    update(_.useCase("UC"))
    scenario(0)
    because(3)

    assertEquals(List(EngineDescription[R, RFn](nodes = List(
      UseCase(title = "UC", nodes = List(
        scenarioObject(params(0)).copyScenario(because = becauseCodeHolder(3))))))),
      currentBuilder.nodes)
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

