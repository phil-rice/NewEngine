package org.cddcore.engine
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.engine.builder._
import org.cddcore.utilities._

@RunWith(classOf[JUnitRunner])
abstract class EngineCddDisplayProcessorTest[Params, BFn, R, RFn, B <: Builder[Params, BFn, R, RFn, R, B, E], E <: EngineTools[Params, BFn, R, RFn]]
  extends DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, R, B, E] {
  import EngineTools._
  import ReportableHelper._
  val ldp2 = CddDisplayProcessor(ClassFunction[String, String](classOf[String], (ldp, s) => s"{$s}"))

  "A builder with a CddDisplayProcessor" should "pass it to the Engine it builds" in {
    resetBuilder(ldp2)
    assert { currentBuilder.ldp.eq(ldp2) }
    val e = build
    assert { e.ldp.eq(ldp2) }
  }

  def expectedPrettyParams: String
  it should "create scenarios that have it and  use it in their pretty print methods" in {
    resetBuilder(ldp2)
    scenario("A")
    expected("X")
    val e = build
    val s = e.asRequirement.scenarios(0)

    assertEquals(ldp2, s.ldp)

    assertEquals(expectedPrettyParams, s.prettyPrintParams)

  }

}
abstract class EngineCddDisplayProcessor1Test[P, R] extends EngineCddDisplayProcessorTest[P, (P) => Boolean, R, (P) => R, Builder1[P, R, R], Engine1[P, R, R]] with SimpleBuilder1Test[P, R]
abstract class EngineCddDisplayProcessor2Test[P1, P2, R] extends EngineCddDisplayProcessorTest[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Builder2[P1, P2, R, R], Engine2[P1, P2, R, R]] with SimpleBuilder2Test[P1, P2, R]
abstract class EngineCddDisplayProcessor3Test[P1, P2, P3, R] extends EngineCddDisplayProcessorTest[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R, R], Engine3[P1, P2, P3, R, R]] with SimpleBuilder3Test[P1, P2, P3, R]

@RunWith(classOf[JUnitRunner])
class EngineCddDisplayProcessorStringStringTest extends EngineCddDisplayProcessor1Test[String, String] with StringStringTest {
  val expectedPrettyParams = "{A}"
}

@RunWith(classOf[JUnitRunner])
class EngineCddDisplayProcessorStringStringStringTest extends EngineCddDisplayProcessor2Test[String, String, String] with StringStringStringTest {
  val expectedPrettyParams = "{A},{A}"
}

@RunWith(classOf[JUnitRunner])
class EngineCddDisplayProcessorStringStringStringStringTest extends EngineCddDisplayProcessor3Test[String, String, String, String] with StringStringStringStringTest {
  val expectedPrettyParams = "{A},{A},{A}"
}
