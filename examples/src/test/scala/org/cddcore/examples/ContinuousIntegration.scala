package org.cddcore.examples

import org.cddcore.tests.CddContinuousIntegrationTest
import org.junit.runner.RunWith
import org.cddcore.tests.CddContinuousIntegrationRunner
import org.cddcore.examples.folding.Folding

@RunWith(classOf[CddContinuousIntegrationRunner])
class ContinuousIntegrationTest extends CddContinuousIntegrationTest {
  val engines = List(
    TennisScorer.scorer,
    Folding.engine)
}
