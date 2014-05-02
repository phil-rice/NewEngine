package org.cddcore.htmlRendering

import org.cddcore.cddjunit.CddContinuousIntegrationTest
import org.cddcore.cddjunit.CddContinuousIntegrationRunner
import org.junit.runner.RunWith

@RunWith(classOf[CddContinuousIntegrationRunner])
class HtmlRenderingTests extends CddContinuousIntegrationTest {

  val engines = List(HtmlRenderer.titleAndIcon, HtmlRenderer.engineAndDocumentsSingleItemRenderer, HtmlRenderer.engineReport)
}