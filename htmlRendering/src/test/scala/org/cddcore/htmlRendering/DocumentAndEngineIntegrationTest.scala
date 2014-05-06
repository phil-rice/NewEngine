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
class DocumentAndEngineIntegrationTest extends AbstractTest with SomeHoldersForTest {

  "A documentAndEngineReport" should "have report paths that go down the documents then the engines" in {
    val report = eBlankTitleDoc1_DocAndEngineReport
    val docHolder = report.documentHolder
    assertEquals(List(doc1), docHolder.nodes)
    assertEquals(List(
      List(report),
      List(report.documentHolder, report),
      List(doc1, report.documentHolder, report),
      List(report.engineHolder, report),
      List(eBlankTitleDoc1ED, report.engineHolder, report)), 
      report.reportPaths)
  }

  "A documentAndEngineReport's pathsToStartChildAndEnd" should "go through the report, document / engine holders and engines." in {
    val report = eBlankTitleDoc1_DocAndEngineReport
    val actual = Lists.traversableToStartChildEnd(report.reportPaths)
    val expected = List(
      (List(report), Start),
      (List(report.documentHolder, report), Start),
      (List(doc1, report.documentHolder, report), Child),
      (List(report.documentHolder, report), End),
      (List(report.engineHolder, report), Start),
      (List(eBlankTitleDoc1ED, report.engineHolder, report), Child),
      (List(report.engineHolder, report), End),
      (List(report), End))
    //    println(Lists.dumpPathsWithStartChildEnd(actual, (x: Reportable) => x.getClass.getSimpleName))
    //    for ((e, a) <- expected.zipAll(actual, null, null))
    //      assertEquals(e, a)

    assertEquals(expected, actual)
  }

  "A documentAndEngineReport' urlMapPath" should "include the engine descriptions" in {
    val report = eBlankTitleDoc1_DocAndEngineReport
    val expected = List(
      List(report),
      List(report.documentHolder, report),
      List(doc1, report.documentHolder, report),
      List(report.engineHolder, report),
      List(eBlankTitleDoc1ED, report.engineHolder, report))
    val actual = report.urlMapPaths
    for ((e, a) <- expected.zipAll(actual, null, null))
      assertEquals(e, a)
    assertEquals(expected, actual)
  }

}
