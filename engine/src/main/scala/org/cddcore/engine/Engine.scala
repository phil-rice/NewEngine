package org.cddcore.engine

trait Engine[Params, BFn, R, RFn] extends HasExceptionMap[R, RFn] {
  def requirements: BuilderNodeHolder[R, RFn]
  def tree: DecisionTree[Params, BFn, R, RFn]
}

trait EngineAndDecisionTree[Params, BFn, R, RFn] extends Engine[Params, BFn, R, RFn] with DecisionTree[Params, BFn, R, RFn] with DecisionTreeAndExceptions[Params, BFn, R, RFn] {
  def tree: DecisionTree[Params, BFn, R, RFn] = this
}

object Engine {
  /** returns a builder for an engine that implements Function[P,R] */
  def apply[P, R]()(implicit ldp: LoggerDisplayProcessor) = Builder1[P, R, R]()(ldp)
  /** returns a builder for an engine that implements Function2[P1,P2,R] */
  def apply[P1, P2, R]()(implicit ldp: LoggerDisplayProcessor) = Builder2Class[P1, P2, R]()(ldp)
  /** returns a builder for an engine that implements Function3[P1,P2,P3,R] */
  def apply[P1, P2, P3, R]()(implicit ldp: LoggerDisplayProcessor) = Builder3Class[P1, P2, P3, R]()(ldp)
  
  

  def checkAllScenarios[Params, BFn, R, RFn](engine: Engine[Params, BFn, R, RFn]) {
    for (s <- engine.requirements.all(classOf[Scenario[Params, BFn, R, RFn]])) {
      val actual = engine.tree.safeEvaluate(s)
      s.expected match {
        case Some(ex) => if (!Reportable.compare(ex, actual)) throw CameToWrongConclusionScenarioException(ex, actual, s)
        case _ => throw NoExpectedException(s)
      }
    }
  }

  def testing = _testing.get
  private var _testing = new ThreadLocal[Boolean] {
    override def initialValue = false;
  }
  def test[X](x: => X) = {
    _testing.set(true)
    try {
      x
    } finally
      _testing.set(false)
  }

}

