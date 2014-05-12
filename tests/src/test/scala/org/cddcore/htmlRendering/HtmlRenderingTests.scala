package org.cddcore.htmlRendering

import org.cddcore.tests.CddContinuousIntegrationTest
import org.junit.runner.RunWith
import org.cddcore.tests.CddContinuousIntegrationRunner


@RunWith(classOf[CddContinuousIntegrationRunner])
class HtmlRenderingTests extends CddContinuousIntegrationTest {

  //  val engines = List(HtmlRenderer.engineReport)
  val engines = List(
    HtmlRenderer.icon,
    HtmlRenderer.linkAndIcon,
    HtmlRenderer.titleAndIcon,
    HtmlRenderer.engineAndDocumentsSingleItemRenderer,
    HtmlRenderer.engineReportSingleItemRenderer)
}