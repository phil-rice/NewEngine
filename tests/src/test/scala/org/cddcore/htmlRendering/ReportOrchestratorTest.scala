package org.cddcore.htmlRendering

import org.cddcore.engine.AbstractTest
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import SampleContexts._
import java.util.Date
import org.scalatest.BeforeAndAfterAll
import org.cddcore.utilities.Strings
import Strings.url

@RunWith(classOf[JUnitRunner])
class ReportOrchestrationTest extends AbstractTest with BeforeAndAfterAll {
  val writer = new MemoryReportWriter
  val rootUrl = "file:///c:/users/xx/.cdd2"
  val title = "reportTitle"
  val projectRoot = url(rootUrl, title)
  override def beforeAll {
    new ReportOrchestrator(rootUrl, title, List(eBlankTitleDoc1, eWithUsecasesAndScenarios, folding), new Date(), writer).makeReports
  }

  "A ReportOrchestrator" should "print to files for the root report, documents, the engines, usecases and scenarios" in {
    val map = writer.map
    val urls = map.keys.toList.sortBy((x) => x)
    val expected = List(
      "file:///c:/users/xx/.cdd2/reportTitle/index.html",
      "file:///c:/users/xx/.cdd2/reportTitle/EBlankTitle.EngineDescription.html",
      "file:///c:/users/xx/.cdd2/reportTitle/eWithUsecasesAndScenarios.EngineDescription.html",
      "file:///c:/users/xx/.cdd2/reportTitle/eWithUsecasesAndScenarios/useCase0.UseCase.html",
      s"file:///c:/users/xx/.cdd2/reportTitle/eWithUsecasesAndScenarios/useCase0/Scenario${uc0s0.textOrder}.Scenario.html",
      "file:///c:/users/xx/.cdd2/reportTitle/eWithUsecasesAndScenarios/useCase1.UseCase.html",
      s"file:///c:/users/xx/.cdd2/reportTitle/eWithUsecasesAndScenarios/useCase1/Scenario${uc1s1.textOrder}.Scenario.html",
      s"file:///c:/users/xx/.cdd2/reportTitle/eWithUsecasesAndScenarios/useCase1/Scenario${uc1s2.textOrder}.Scenario.html",
      "file:///c:/users/xx/.cdd2/reportTitle/Folding_Engine_Title.FoldingEngineDescription.html",
      "file:///c:/users/xx/.cdd2/reportTitle/Folding_Engine_Title/ce0.EngineDescription.html",
      s"file:///c:/users/xx/.cdd2/reportTitle/Folding_Engine_Title/ce0/Scenario${ce0s0.textOrder}.Scenario.html",
      "file:///c:/users/xx/.cdd2/reportTitle/Folding_Engine_Title/ce1.EngineDescription.html",
      s"file:///c:/users/xx/.cdd2/reportTitle/Folding_Engine_Title/ce1/Scenario${ce1s1.textOrder}.Scenario.html",
      s"file:///c:/users/xx/.cdd2/reportTitle/Folding_Engine_Title/ce1/Scenario${ce1s2.textOrder}.Scenario.html").sortBy((x) => x)
    //    for ((e, a) <- expected.zipAll(urls, null, null))
    //      assertEquals(e, a)
    assertEquals(expected, urls)
  }

  it should "" in {

  }

}