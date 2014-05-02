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

@RunWith(classOf[JUnitRunner])
class DocumentAndEngineIntegrationTest extends AbstractTest with SomeHoldersForTest {

  "A report on documents and engines pathsToStartChildAndEnd" should "go through the report, document / engine holders and engines." in {
    import SampleContexts._
    import StartChildEndType._
    val report = Report(nodes = List(documentAndEngineHolder))
    println("------------------------------")
    println(Lists.dumpPathsWithStartChildEnd(Lists.pathToStartChildEnd(report.pathsIncludingSelf), (x: Any) => x.getClass.getSimpleName))
    println("------------------------------")
    assertEquals(List(
      (List(report), Start),
      (List(documentAndEngineHolder, report), Start),
      (List(documentAndEngineHolder.documentHolder, documentAndEngineHolder, report), Start),
      (List(doc1, documentAndEngineHolder.documentHolder, documentAndEngineHolder, report), Child),
      (List(documentAndEngineHolder.documentHolder, documentAndEngineHolder, report), End),
      (List(documentAndEngineHolder.engineHolder, documentAndEngineHolder, report), Start),
      (List(eBlankWithTitle, documentAndEngineHolder.engineHolder, documentAndEngineHolder, report), Child),
      (List(documentAndEngineHolder.engineHolder, documentAndEngineHolder, report), End),
      (List(documentAndEngineHolder, report), End),
      (List(report), End)),
      Lists.pathToStartChildEnd(report.pathsIncludingSelf))
  }

}
