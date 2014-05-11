package org.cddcore.engine

import org.junit.runner.RunWith;
import org.scalatest.junit.JUnitRunner

import org.cddcore.engine.builder._

@RunWith(classOf[JUnitRunner])
class DecisionTreeSmokeTest extends AbstractTest {

  "A decision tree findPathToConclusion method " should "return the conclusion, and it's parent decisions" in {
    import scala.language.implicitConversions
    implicit def toEngineFromTests(e: Engine1[String, String, String]) = e.asInstanceOf[Engine1FromTests[String, String]]
    implicit def todecision[Params, BFn, R, RFn](x: DecisionTreeNode[Params, BFn, R, RFn]) = x.asInstanceOf[Decision[Params, BFn, R, RFn]]
    implicit def toConclusion[Params, BFn, R, RFn](x: DecisionTreeNode[Params, BFn, R, RFn]) = x.asInstanceOf[Conclusion[Params, BFn, R, RFn]]

    val e = Engine[String, String]().
      scenario("A").expected("X").because { _.contains("A") }.
      scenario("AB").expected("Y").because { _.contains("B") }.
      scenario("ABC").expected("Z").because { _.contains("C") }.
      scenario("B").expected("Q").because { _.contains("B") }.
      build

    val tree = e.tree
    val d_a = tree.root
    val d_ab = d_a.yes
    val d_abc = d_ab.yes
    val c_y = d_abc.no
    val c_z = d_abc.yes
    val c_x = d_ab.no
    val d_b = d_a.no
    val c_q = d_b.yes
    val c_ex = d_b.no

    assertEquals(c_ex :: d_b :: d_a :: Nil, e.evaluator.findPathToConclusion(e.tree, "R"))
    val actual = e.evaluator.findPathToConclusion(e.tree, "AB")
    assertEquals(c_y :: d_abc :: d_ab :: d_a :: Nil, actual)

  }

}
