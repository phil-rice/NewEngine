package org.cddcore.engine

trait Engine[Params, BFn, R, RFn] {
  def requirements: BuilderNodeHolder[R, RFn]
  def evaluator: EvaluateTree[Params, BFn, R, RFn]
  def buildExceptions: Map[BuilderNode[R, RFn], List[Exception]]
}
trait EngineFromTests[Params, BFn, R, RFn] extends Engine[Params, BFn, R, RFn] {
  def tree: DecisionTree[Params, BFn, R, RFn]
  def applyParams(params: Params): R
}

object Engine {
  /** returns a builder for an engine that implements Function[P,R] */
  def apply[P, R]()(implicit ldp: LoggerDisplayProcessor) = Builder1[P, R, R](BuildEngine.initialNodes, Map(), BuildEngine.builderEngine1)(ldp)
  /** returns a builder for an engine that implements Function2[P1,P2,R] */
  def apply[P1, P2, R]()(implicit ldp: LoggerDisplayProcessor) = Builder2[P1, P2, R, R](BuildEngine.initialNodes, Map(), BuildEngine.builderEngine2)(ldp)
  /** returns a builder for an engine that implements Function3[P1,P2,P3,R] */
  def apply[P1, P2, P3, R]()(implicit ldp: LoggerDisplayProcessor) = Builder3[P1, P2, P3, R, R](BuildEngine.initialNodes, Map(), BuildEngine.builderEngine3)(ldp)

  def folding[P, R, FullR](initialValue: FullR, foldingFn: (FullR, R) => FullR)(implicit ldp: LoggerDisplayProcessor) =
    Builder1[P, R, FullR](BuildEngine.initialNodes(initialValue, foldingFn), Map(), BuildEngine.folderBuilderEngine1[P, R, FullR])(ldp)
  def foldList[P, R] = folding[P, R, List[R]](List(), (acc: List[R], v: R) => acc :+ v)
  def foldSet[P, R] = folding[P, R, Set[R]](Set(), { _ + _ })

  def folding[P1, P2, R, FullR](initialValue: FullR, foldingFn: (FullR, R) => FullR)(implicit ldp: LoggerDisplayProcessor) =
    Builder2[P1, P2, R, FullR](BuildEngine.initialNodes(initialValue, foldingFn), Map(), BuildEngine.folderBuilderEngine2[P1, P2, R, FullR])(ldp)
  def foldList[P1, P2, R] = folding[P1, P2, R, List[R]](List(), (acc: List[R], v: R) => acc :+ v)
  def foldSet[P1, P2, R] = folding[P1, P2, R, Set[R]](Set(), { _ + _ })

  def folding[P1, P2, P3, R, FullR](initialValue: FullR, foldingFn: (FullR, R) => FullR)(implicit ldp: LoggerDisplayProcessor) =
    Builder3[P1, P2, P3, R, FullR](BuildEngine.initialNodes(initialValue, foldingFn), Map(), BuildEngine.folderBuilderEngine3[P1, P2, P3, R, FullR])(ldp)
  def foldList[P1, P2, P3, R] = folding[P1, P2, P3, R, List[R]](List(), (acc: List[R], v: R) => acc :+ v)
  def foldSet[P1, P2, P3, R] = folding[P1, P2, P3, R, Set[R]](Set(), { _ + _ })

  def checkAllScenarios[Params, BFn, R, RFn](engine: EngineFromTests[Params, BFn, R, RFn]) {
    for (s <- engine.requirements.all(classOf[Scenario[Params, BFn, R, RFn]])) {
      val actual = engine.evaluator.safeEvaluate(engine.tree, s)
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

