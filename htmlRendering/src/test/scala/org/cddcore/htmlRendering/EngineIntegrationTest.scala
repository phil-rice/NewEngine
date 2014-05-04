package org.cddcore.htmlRendering

import scala.language.implicitConversions
import org.cddcore.engine._
import org.cddcore.engine.builder.Decision
import org.cddcore.engine.builder.DecisionTreeNode
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import ReportableHelper._
import org.cddcore.utilities.NestedHolder
import org.cddcore.utilities.Lists
import org.cddcore.utilities.StartChildEndType
import SampleContexts._
import StartChildEndType._

@RunWith(classOf[JUnitRunner])
class EngineIntegrationTest extends AbstractTest with SomeHoldersForTest {

  "EngineReport" should "have report paths that goes down the engineDescription and any children" in {
    val x = ed
    val ucs = List(uc1, uc0)
    assertEquals(List(uc0, uc1), ed.nodes)

    val report = engineReport
    val expected = List(
      List(report),
      List(ed, report),
      List(uc0, ed, report),
      List(uc0s0, uc0, ed, report),
      List(uc1, ed, report),
      List(uc1s1, uc1, ed, report),
      List(uc1s2, uc1, ed, report))
    val actual = report.reportPaths
    for ((e, a) <- expected.zipAll(actual, null, null))
      assertEquals(e, a)
    assertEquals(expected, actual)
  }

  "EngineReport's pathsToStartChildAndEnd" should "go through the report, document / engine holders and engines." in {
    val report = engineReport
    val actual = Lists.traversableToStartChildEnd(report.reportPaths)
    val expected = List(
      (List(report), Start),
      (List(ed, report), Start),
      (List(uc0, ed, report), Start),
      (List(uc0s0, uc0, ed, report), Child),
      (List(uc0, ed, report), End),
      (List(uc1, ed, report), Start),
      (List(uc1s1, uc1, ed, report), Child),
      (List(uc1s2, uc1, ed, report), Child),
      (List(uc1, ed, report), End),
      (List(ed, report), End),
      (List(report), End))

    assertEquals(expected, actual)
  }

}
