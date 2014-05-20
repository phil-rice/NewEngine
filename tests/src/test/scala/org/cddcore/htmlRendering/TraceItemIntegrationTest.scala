package org.cddcore.htmlRendering

import scala.language.implicitConversions

import org.cddcore.engine._
import org.cddcore.engine.builder._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.cddcore.utilities._

import SampleContexts._
import StartChildEndType._
import ReportableHelper._
import EngineTools._
import ReportableHelper._

@RunWith(classOf[JUnitRunner])
class TraceItemIntegrationTest extends AbstractTest {

  import SampleContexts._

  "TraceReport" should "generate report paths based on trace items" in {

    //    val expected = List(List(fullTI), List(ce0TI, fullTI), List(ce1TI, fullTI))
    val report = foldingTraceReport
    val actual = report.reportPaths
    val expected: List[List[Reportable]] = List(
      List(report),
      List(foldingTI, report),
      List(ce0TI, foldingTI, report),

      List(ce0s0,  ce0TI, foldingTI, report),
      List(ce0Tree,  ce0TI, foldingTI, report),
      List(concCe0, ce0Tree,  ce0TI, foldingTI, report),
      List(ce1TI, foldingTI, report),
      List(ce1s1,  ce1TI, foldingTI, report),
      List(ce1s2,  ce1TI, foldingTI, report),
      List(ce1Tree,  ce1TI, foldingTI, report),
      List(decisionCe1, ce1Tree,  ce1TI, foldingTI, report),
      List(concYesCe1, decisionCe1, ce1Tree,  ce1TI, foldingTI, report),
      List(ElseClause(), decisionCe1, ce1Tree,  ce1TI, foldingTI, report),
      List(concNoCe1, decisionCe1, ce1Tree,  ce1TI, foldingTI, report))
    for ((e, a) <- expected.zipAll(actual, null, null))
      if (e != a) {
        val compare = e.map(_.getClass.getSimpleName).zipAll(a.map(_.getClass.getSimpleName), null, null)
        assertEquals(e, a)
      }
    assertEquals(expected, actual)
        println(Report.htmlFromTrace("title", trace))
  }

  it should "generate urlMapPaths based in traceItems and the requirements of the engines referenced" in {
    val actual = foldingTraceReport.urlMapPaths
    val expected = List(
      List(foldingED),
      List(ce0ED, foldingED),
      List(ce0s0, ce0ED, foldingED),
      List(ce0Tree, ce0ED, foldingED),
      List(concCe0, ce0Tree, ce0ED, foldingED),
      List(ce1ED, foldingED),
      List(ce1s1, ce1ED, foldingED),
      List(ce1s2, ce1ED, foldingED),
      List(ce1Tree, ce1ED, foldingED),
      List(decisionCe1, ce1Tree, ce1ED, foldingED),
      List(concYesCe1, decisionCe1, ce1Tree, ce1ED, foldingED),
      List(ElseClause(), decisionCe1, ce1Tree, ce1ED, foldingED),
      List(concNoCe1, decisionCe1, ce1Tree, ce1ED, foldingED),
      List(foldingTI),
      List(ce0TI, foldingTI),
      List(ce1TI, foldingTI))
    for ((e, a) <- expected.zipAll(actual, null, null))
      assertEquals(e, a)
    assertEquals(expected, actual)

  }

}
