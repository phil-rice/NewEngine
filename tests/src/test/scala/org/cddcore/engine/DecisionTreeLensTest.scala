package org.cddcore.engine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions

object DecisionTreeLensTest

abstract class DecisionTreeLensTest[Params, BFn, R, RFn, B <: Builder[R, RFn, B], E <: Engine[Params, BFn, R, RFn]] extends BuilderTest[Params, BFn, R, RFn, B, E] {
  implicit def toBuilderWithModifyChildrenForBuild[R, RFn](b: B) = b.asInstanceOf[BuilderWithModifyChildrenForBuild[R, RFn]]
  implicit def toSome[X](x: X) = Some(x)
}

abstract class DecisionTreeLens1Test[P, R] extends DecisionTreeLensTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R], Engine1[P, R]] with Builder1Test[P, R]
abstract class DecisionTreeLens2Test[P1, P2, R] extends DecisionTreeLensTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R], Engine2[P1, P2, R]] with Builder2Test[P1, P2, R]
abstract class DecisionTreeLens3Test[P1, P2, P3, R] extends DecisionTreeLensTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R], Engine3[P1, P2, P3, R]] with Builder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class DecisionTreeLensStringStringTest extends DecisionTreeLens1Test[String, String] with StringStringTest

@RunWith(classOf[JUnitRunner])
class DecisionTreeLensStringStringStringTest extends DecisionTreeLens2Test[String, String, String] with StringStringStringTest

@RunWith(classOf[JUnitRunner])
class DecisionTreeLensStringStringStringStringTest extends DecisionTreeLens3Test[String, String, String, String] with StringStringStringStringTest
