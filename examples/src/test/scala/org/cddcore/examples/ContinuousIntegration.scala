package org.cddcore.examples

import org.cddcore.cddjunit.CddContinuousIntegrationTest
import org.cddcore.examples.folding.Folding
import org.junit.runner.RunWith
import org.cddcore.cddjunit.CddContinuousIntegrationRunner

@RunWith(classOf[CddContinuousIntegrationRunner])
class ContinuousIntegrationTest extends CddContinuousIntegrationTest {
  val engines = List(
    TennisScorer.scorer,
    Folding.engine)
}
