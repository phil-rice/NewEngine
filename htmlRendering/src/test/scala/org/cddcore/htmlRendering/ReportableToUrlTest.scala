package org.cddcore.htmlRendering

import org.junit.runner.RunWith
import org.cddcore.engine._
import org.scalatest.junit.JUnitRunner
import scala.language.implicitConversions
import org.cddcore.engine.builder.DecisionTreeNode
import org.cddcore.engine.builder.Decision

@RunWith(classOf[JUnitRunner])
class ReportableToUrlTest extends AbstractTest with SomeHoldersForTest with ReportableTestFramework {

  val rep1a = Req("rep1", "d2")
  val rep1b = Req("rep1", "d3")

  "A ReportableToUrl" should "produce a unique human readable name for each reportable" in {
    val reportableToUrl = new SimpleReportableToUrl
    assertEquals("rep1", reportableToUrl(rep1))
    assertEquals("rep1", reportableToUrl(rep1))
    assertEquals("rep2", reportableToUrl(rep2))
    assertEquals("rep2", reportableToUrl(rep2))
    assertEquals("rep3", reportableToUrl(rep3))
    assertEquals("rep3", reportableToUrl(rep3))
    assertEquals("rep4", reportableToUrl(rep4))
    assertEquals("rep4", reportableToUrl(rep4))
    assertEquals("rep5", reportableToUrl(rep5))
    assertEquals("rep5", reportableToUrl(rep5))
    assertEquals("Req6", reportableToUrl(rep1a))
    assertEquals("Req7", reportableToUrl(rep1b))
    assertEquals("Req6", reportableToUrl(rep1a))
    assertEquals("Req7", reportableToUrl(rep1b))
  }

  it should "amalgamate all the referenced documents" in {
    val reportableToUrl = new SimpleReportableToUrl
    val urlMap = reportableToUrl.makeUrlMap(holderD12)
    assert(!urlMap.contains(doc0))
    assert(urlMap.contains(doc1))
    assert(urlMap.contains(doc2))

    assertEquals("/Holder1/name1.Document.html", urlMap.apply(doc1))
    assertEquals("/Holder1/name2.Document.html", urlMap.apply(doc2))
  }

  it should "combine the path from the human readable names with the apply method" in {
    val reportableToUrl = new SimpleReportableToUrl
    assertEquals("rep2/rep1", reportableToUrl(List(rep1, rep2)))
    assertEquals("rep2:rep1", reportableToUrl(List(rep1, rep2), separator = ":"))
    assertEquals("Req3:rep1", reportableToUrl(List(rep1, rep1b), separator = ":"))
  }

  it should "make a urlId from the template name, human readable name with optional suffix" in {
    val reportableToUrl = new SimpleReportableToUrl
    assertEquals("Req_rep1", reportableToUrl.urlId(rep1))
    assertEquals("Req_Req2", reportableToUrl.urlId(rep1a))
  }

  it should "make a url from the template name of the head with path" in {
    val reportableToUrl = new SimpleReportableToUrl
    assertEquals(Some("/rep1/Req2.Req.html"), reportableToUrl.url(List(rep1a, rep1)))
    assertEquals(Some("/Holder3/rep1.Req.html"), reportableToUrl.url(List(rep1, holder12)))
    assertEquals(Some("/rep1/Holder3.Holder.html"), reportableToUrl.url(List(holder12, rep1)))
  }

  it should "make a urlMap from all the reportables in the container" in {
    val reportableToUrl = new SimpleReportableToUrl
    val urlMap = reportableToUrl.makeUrlMap(holder12)
    checkUrl(reportableToUrl, urlMap, holder12);
    checkUrl(reportableToUrl, urlMap, rep1, holder12);
    checkUrl(reportableToUrl, urlMap, rep2, holder12);
    assertEquals(3, urlMap.size)
  }

  it should "make a urlMap from engine " in {
    val e = Engine[Int, String]().useCase("").scenario(1).expected("x").build
    val ed = e.asRequirement
    val reportableToUrl = new SimpleReportableToUrl
    val urlMap = reportableToUrl.makeUrlMapFor(e)
    val uc1 = e.asRequirement.all(classOf[UseCase[String, (Int) => String]])(0)
    val s1 = e.asRequirement.scenarios(0)
    assertEquals(4, urlMap.size)
    checkUrl(reportableToUrl, urlMap, e)
    checkUrl(reportableToUrl, urlMap, ed)
    checkUrl(reportableToUrl, urlMap, uc1, ed)
    checkUrl(reportableToUrl, urlMap, s1, uc1, ed)
  }

  it should "make a urlMap from reportables and decision / conclusions " in {
    val e = Engine[Int, String]().
      useCase("").scenario(1).expected("x").
      useCase("").
      scenario(2).
      expected("y").
      because((x: Int) => x == 2).
      build
    val reportableToUrl = new SimpleReportableToUrl

    val urlMap = reportableToUrl.makeUrlMapWithDecisionsAndConclusions(Holder("EngineHolder", List(e)))
    val uc1 = e.asRequirement.useCases(0)
    val uc2 = e.asRequirement.useCases(1)
    val s1 = e.asRequirement.scenarios(0)
    val s2 = e.asRequirement.scenarios(1)
    val tree =  e.asInstanceOf[EngineFromTests[Int, (Int) =>Boolean, String, (Int)=>String]].tree
    import tree._
    val d = tree.root.asInstanceOf[Decision[Int, (Int) =>Boolean, String, (Int)=>String]]
    val c1 = d.yes
    val c2 = d.no
    assertEquals(8, urlMap.size)
    checkUrl(reportableToUrl, urlMap, e)
    checkUrl(reportableToUrl, urlMap, uc1, e)
    checkUrl(reportableToUrl, urlMap, s1, uc1, e)
    checkUrl(reportableToUrl, urlMap, uc2, e)
    checkUrl(reportableToUrl, urlMap, s2, uc2, e)
    checkUrl(reportableToUrl, urlMap, d, e)
    checkUrl(reportableToUrl, urlMap, c1, e)
    checkUrl(reportableToUrl, urlMap, c2, e)

  }

  def checkUrl(reportableToUrl: ReportableToUrl, urlMap: UrlMap, r: Reportable*) {
    assertEquals(reportableToUrl.url(r.toList).get, urlMap(r.head))
  }
}