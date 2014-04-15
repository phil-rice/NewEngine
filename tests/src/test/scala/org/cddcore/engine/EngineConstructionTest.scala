package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions

abstract class EngineConstructionTest[Params, BFn, R, RFn, B <: Builder[R, RFn, B], E <: Engine[Params, BFn, R, RFn]] extends BuilderTest[Params, BFn, R, RFn, B, E] with DecisionTreeBuilder[Params, BFn, R, RFn] {
  implicit def toBuilderWithModifyChildrenForBuild[R, RFn](b: B) = b.asInstanceOf[BuilderWithModifyChildrenForBuild[R, RFn]]
  implicit def toSome[X](x: X) = Some(x)
  implicit def toDecisionTreeDecisionTree[Params, BFn, R, RFn](x: Engine[Params, BFn, R, RFn]) = x.asInstanceOf[DecisionTree[Params, BFn, R, RFn]]

  s"A blank $builderName " should "just have the default root" in {
    val engine = build
    val root = engine.root
    assertEquals(defaultRoot, root)
  }

  builderName  should "throw  CannotDefineTitleTwiceException if the title has already been set" in {
    scenario("A")
    update(_.title("X"))
    evaluating { update(_.title("X")) } should produce[CannotDefineTitleTwiceException]
  }
  it  should "throw  CannotDefineDescriptionTwiceException if the description has already been set" in {
    scenario("A")
    update(_.description("X"))
    evaluating { update(_.description("X")) } should produce[CannotDefineDescriptionTwiceException]
  }
 it should "throw  CannotDefinePriorityTwiceException if the priority has already been set" in {
    scenario("A")
    update(_.priority(1))
    evaluating { update(_.priority(1)) } should produce[CannotDefinePriorityTwiceException]
  }
 it should "throw  CannotDefineBecauseTwiceException if the because has already been set" in {
    scenario("A")
    because("A")
    evaluating { because("A") } should produce[CannotDefineBecauseTwiceException]
  }

 it should "throw  CannotDefineExpectedTwiceException if the expected has already been set" in {
    scenario("A")
    expected("X")
    evaluating { expected("X") } should produce[CannotDefineExpectedTwiceException]
  }
 it should "throw  CannotDefineCodeTwiceException if the code has already been set" in {
    scenario("A")
    code("X")
    evaluating { code("X") } should produce[CannotDefineCodeTwiceException]
  }
}

abstract class EngineConstruction1Test[P, R] extends EngineConstructionTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R], Engine1[P, R]] with Builder1Test[P, R]
abstract class EngineConstruction2Test[P1, P2, R] extends EngineConstructionTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R], Engine2[P1, P2, R]] with Builder2Test[P1, P2, R]
abstract class EngineConstruction3Test[P1, P2, P3, R] extends EngineConstructionTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R], Engine3[P1, P2, P3, R]] with Builder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineConstructionStringStringTest extends EngineConstruction1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class EngineConstructionStringStringStringTest extends EngineConstruction2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class EngineConstructionStringStringStringStringTest extends EngineConstruction3Test[String, String, String, String] with StringStringStringStringTest
