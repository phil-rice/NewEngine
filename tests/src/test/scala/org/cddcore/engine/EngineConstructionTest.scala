package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions

abstract class EngineConstructionTest[Params, BFn, R, RFn, B <: Builder[R, RFn, B], E <: Engine[Params, BFn, R, RFn]] extends BuilderTest[Params, BFn, R, RFn, B, E] {
  implicit def toBuilderWithModifyChildrenForBuild[R, RFn](b: B) = b.asInstanceOf[BuilderWithModifyChildrenForBuild[R, RFn]]
  implicit def toSome[X](x: X) = Some(x)
  implicit def toDecisionTreeDecisionTree[Params, BFn, R, RFn](x: Engine[Params, BFn, R, RFn]) = x.asInstanceOf[DecisionTree[Params, BFn, R, RFn]]

  "A blank engine" should "just have the default root" in {
    val engine = build
    val root = engine.root
    assertEquals(defaultRoot, root)
  }
  
}

abstract class EngineConstruction1Test[P, R] extends EngineConstructionTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R], Engine1[P, R]] with Builder1Test[P, R]
abstract class EngineConstruction2Test[P1, P2, R] extends EngineConstructionTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R], Engine2[P1, P2, R]] with Builder2Test[P1, P2, R]
abstract class EngineConstruction3Test[P1, P2, P3, R] extends EngineConstructionTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R], Engine3[P1, P2, P3, R]] with Builder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineConstructionStringIntTest extends EngineConstruction1Test[String, Int] with StringIntTest

@RunWith(classOf[JUnitRunner])
class EngineConstructionStringStringTest extends EngineConstruction1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class EngineConstructionStringStringStringTest extends EngineConstruction2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class EngineConstructionStringStringStringStringTest extends EngineConstruction3Test[String, String, String, String] with StringStringStringStringTest
