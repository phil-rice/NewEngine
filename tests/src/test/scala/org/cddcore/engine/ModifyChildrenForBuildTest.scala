package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions

object ModifyChildrenForBuildTest

abstract class ModifyChildrenForBuildTest[Params, BFn, R, RFn, B <: Builder[R, RFn, B], E <: Engine[Params, BFn, R, RFn]] extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, B, E] {
  implicit def toBuilderWithModifyChildrenForBuild[R, RFn](b: B) = b.asInstanceOf[BuilderWithModifyChildrenForBuild[R, RFn]]
  implicit def toSome[X](x: X) = Some(x)
  "Scenarios when modifiedForBuild" should "inherit priority from parent if not defined" in {
    update(_.priority(2).useCase("UC").priority(1))
    scenario("A")
    assertEquals(ModifedChildrenBuilderNodeHolder(List(EngineDescription[R, RFn](priority = 2, nodes = List(
      UseCase(title = "UC", priority = 1, nodes = List(s("A", priority = 1))))))),
      currentBuilder.modifyChildrenForBuild)
  }
  it should "use own priority if defined" in {
    update(_.priority(2).useCase("UC").priority(1))
    scenario("A")
    update(_.priority(3))
    assertEquals(ModifedChildrenBuilderNodeHolder(List(EngineDescription[R, RFn](priority = 2, nodes = List(
      UseCase(title = "UC", priority = 1, nodes = List(
        s("A", priority = 3))))))),
      currentBuilder.modifyChildrenForBuild)
  }
  it should "inherit expected from parent if not defined" in {
    update(_.expected(result("X")).useCase("UC").expected(result("Y")))
    scenario("A")
    assertEquals(ModifedChildrenBuilderNodeHolder(List(EngineDescription[R, RFn](expected = Right(result("X")), nodes = List(
      UseCase(title = "UC", expected = Right(result("Y")), nodes = List(s("A", expected = "Y"))))))),
      currentBuilder.modifyChildrenForBuild)
  }

  it should "use own  expected if  defined" in {
    update(_.expected(result("X")).useCase("UC").expected(result("Y")))
    scenario("A")
    update(_.expected(result("Z")))
    assertEquals(ModifedChildrenBuilderNodeHolder(List(EngineDescription[R, RFn](expected = Right(result("X")), nodes = List(
      UseCase(title = "UC", expected = Right(result("Y")), nodes = List(
        s("A", expected = "Z"))))))),
      currentBuilder.modifyChildrenForBuild)
  }
  it should "inherit code from parent if not defined" in {
    code("X")
    update(_.useCase("UC"))
    code("Y")
    scenario("A")

    assertEquals(ModifedChildrenBuilderNodeHolder(List(EngineDescription[R, RFn](code = resultCodeHolder("X"), nodes = List(
      UseCase(title = "UC", code = resultCodeHolder("Y"), nodes = List(
        s("A", code = resultCodeHolder("Y")))))))),
      currentBuilder.modifyChildrenForBuild)
  }

  it should "use own  code if  defined" in {
    code("X")
    update(_.useCase("UC"))
    code("Y")
    scenario("A")
    code("Z")

    assertEquals(ModifedChildrenBuilderNodeHolder(List(EngineDescription[R, RFn](code = resultCodeHolder("X"), nodes = List(
      UseCase(title = "UC", code = resultCodeHolder("Y"), nodes = List(
        s("A", code = resultCodeHolder("Z")))))))),
      currentBuilder.modifyChildrenForBuild)
  }

  it should "sort children by initially order they were defined in, but by priority if specified" in {
    update(_.useCase("UC1").priority(1).useCase("UC2").priority(2).useCase("UC3").useCase("UC4"))
    assertEquals(ModifedChildrenBuilderNodeHolder(List(EngineDescription[R, RFn](nodes = List(
      UseCase(title = "UC2", priority = 2),
      UseCase(title = "UC1", priority = 1),
      UseCase(title = "UC3"),
      UseCase(title = "UC4"))))),
      currentBuilder.modifyChildrenForBuild)
  }



}

abstract class ModifyChildrenForBuild1Test[P, R] extends ModifyChildrenForBuildTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R, R], Engine1[P, R]] with Builder1Test[P, R, R]
abstract class ModifyChildrenForBuild2Test[P1, P2, R] extends ModifyChildrenForBuildTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R], Engine2[P1, P2, R]] with Builder2Test[P1, P2, R]
abstract class ModifyChildrenForBuild3Test[P1, P2, P3, R] extends ModifyChildrenForBuildTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R], Engine3[P1, P2, P3, R]] with Builder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class ModifyChildrenForBuildStringStringTest extends ModifyChildrenForBuild1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class ModifyChildrenForBuildStringStringStringTest extends ModifyChildrenForBuild2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class ModifyChildrenForBuildStringStringStringStringTest extends ModifyChildrenForBuild3Test[String, String, String, String] with StringStringStringStringTest
