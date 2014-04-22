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

  it should "give things with an empty string for a name (after cleaning) an sensible name" in {
    val reportableToUrl = new SimpleReportableToUrl
    val name = reportableToUrl(repEmpty);
    assertEquals("Req1", name)
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

    val holder = Holder("EngineHolder", List(e))
    val urlMap = reportableToUrl.makeUrlMapWithDecisionsAndConclusions(holder)
    val uc1 = e.asRequirement.useCases(0)
    val uc2 = e.asRequirement.useCases(1)
    val s1 = e.asRequirement.scenarios(0)
    val s2 = e.asRequirement.scenarios(1)
    val tree = e.asInstanceOf[EngineFromTests[Int, (Int) => Boolean, String, (Int) => String]].tree
    import tree._
    val d = tree.root.asInstanceOf[Decision[Int, (Int) => Boolean, String, (Int) => String]]
    val c1 = d.yes
    val c2 = d.no
    val ed = e.asRequirement
    checkUrl(reportableToUrl, urlMap, holder)
    checkUrl(reportableToUrl, urlMap, e, holder)
    checkUrl(reportableToUrl, urlMap, d, e, holder)
    checkUrl(reportableToUrl, urlMap, c1, d, e, holder)
    checkUrl(reportableToUrl, urlMap, c2, d, e, holder)

    checkUrl(reportableToUrl, urlMap, ed, holder)
    checkUrl(reportableToUrl, urlMap, uc1, ed, holder)
    checkUrl(reportableToUrl, urlMap, s1, uc1, ed, holder)
    checkUrl(reportableToUrl, urlMap, uc2, ed, holder)
    checkUrl(reportableToUrl, urlMap, s2, uc2, ed, holder)
    assertEquals(10, urlMap.size)
  }

  it should "make a urlMap for folding engines" in {
    implicit def toFoldingEngine[Params, BFn, R, RFn, Full](e: Engine[Params, BFn, R, RFn]) = e.asInstanceOf[FoldingEngine[Params, BFn, R, RFn, Full]]
    val f = Engine.foldList[Int, String].
      childEngine("").useCase("uc1").scenario(1).expected("x").scenario(2).expected("y"). because((x: Int) => x == 2).
      childEngine("").
      scenario(1).expected("x").scenario(2).expected("y"). because((x: Int) => x == 2).
      build
    val reportableToUrl = new SimpleReportableToUrl

    val holder = Holder("EngineHolder", List(f))
    val urlMap = reportableToUrl.makeUrlMapWithDecisionsAndConclusions(holder)
    val ce1 = f.engines(0)
    val ce2 = f.engines(1)
    val uc1 = f.asRequirement.useCases(0)
    val s11 = ce1.asRequirement.scenarios(0)
    val s12 = ce1.asRequirement.scenarios(1)
    val s21 = ce2.asRequirement.scenarios(0)
    val s22 = ce2.asRequirement.scenarios(1)
    val d1 = ce1.tree.root.asInstanceOf[Decision[Int, (Int) => Boolean, String, (Int) => String]]
    val c11 = d1.yes
    val c12 = d1.no
    val d2 = ce2.tree.root.asInstanceOf[Decision[Int, (Int) => Boolean, String, (Int) => String]]
    val c21 = d2.yes
    val c22 = d2.no

    val fed = f.asRequirement
    val ed1 = ce1.asRequirement
    val ed2 = ce2.asRequirement
    checkUrl(reportableToUrl, urlMap, holder)
    checkUrl(reportableToUrl, urlMap, f, holder)
    checkUrl(reportableToUrl, urlMap, ce1, f, holder)
    checkUrl(reportableToUrl, urlMap, d1, ce1, f, holder)
    checkUrl(reportableToUrl, urlMap, c11, d1, ce1, f, holder)
    checkUrl(reportableToUrl, urlMap, c12, d1, ce1, f, holder)
    checkUrl(reportableToUrl, urlMap, ce2, f, holder)
    checkUrl(reportableToUrl, urlMap, d2, ce2, f, holder)
    checkUrl(reportableToUrl, urlMap, c21, d2, ce2, f, holder)
    checkUrl(reportableToUrl, urlMap, c22, d2, ce2, f, holder)

    checkUrl(reportableToUrl, urlMap, fed, holder)
    checkUrl(reportableToUrl, urlMap, ed1, fed, holder)
    checkUrl(reportableToUrl, urlMap, uc1, ed1, fed, holder)
    checkUrl(reportableToUrl, urlMap, s11, uc1, ed1, fed, holder)
    checkUrl(reportableToUrl, urlMap, s12, uc1, ed1, fed, holder)
    checkUrl(reportableToUrl, urlMap, ed2, fed, holder)
    checkUrl(reportableToUrl, urlMap, s21, ed2, fed, holder)
    checkUrl(reportableToUrl, urlMap, s22, ed2, fed, holder)
    assertEquals(18, urlMap.size)
  }

  def checkUrl(reportableToUrl: ReportableToUrl, urlMap: UrlMap, r: Reportable*) {
    assertEquals(reportableToUrl.url(r.toList).get, urlMap(r.head))
  }
}