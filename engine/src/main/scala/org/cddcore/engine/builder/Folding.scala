package org.cddcore.engine.builder

import org.cddcore.engine._
import org.cddcore.utilities._




trait BuildFoldingEngine[Params, BFn, R, RFn, FullR, F <: Engine[Params, BFn, R, RFn], E <: Engine[Params, BFn, R, RFn]] extends BuildEngine[Params, BFn, R, RFn, FullR, F] {
  def constructFoldingEngine(
    requirement: BuilderNodeAndHolder[R, RFn],
    engines: List[EngineFromTests[Params, BFn, R, RFn]],
    exceptionMap: ExceptionMap,
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR): F
  def buildChildEngine: BuildEngine[Params, BFn, R, RFn, R, E]

  def buildEngine(r: BuilderNodeAndHolder[R, RFn], buildExceptions: ExceptionMap) = {
    r match {
      case f: FoldingEngineDescription[R, RFn, FullR] => {
        val initial = (List[EngineFromTests[Params, BFn, R, RFn]](), buildExceptions)
        if (f.nodes.isEmpty) throw new CannotHaveFoldingEngineWithoutChildEnginesException
        val (engines: List[EngineFromTests[Params, BFn, R, RFn]], exceptionMap) = f.nodes.foldLeft(initial)((acc, ed) => ed match {
          case ed: EngineDescription[R, RFn] => {
            val (engines, initialExceptionMap) = acc
            val (dt, exceptionMap, newRequirements) = buildTree(ed, initialExceptionMap)
            val engine = buildChildEngine.buildEngine(newRequirements, buildExceptions).asInstanceOf[EngineFromTests[Params, BFn, R, RFn]]
            (engine :: engines, exceptionMap)
          }
        });
        constructFoldingEngine(f, engines, exceptionMap, f.initialValue, f.foldingFn)
      }
    }
  }
}

abstract class SimpleFoldingBuildEngine[Params, BFn, R, RFn, FullR, F <: Engine[Params, BFn, R, RFn], E <: Engine[Params, BFn, R, RFn]](
  val root: DecisionTreeNode[Params, BFn, R, RFn],
  makeClosures: MakeClosures[Params, BFn, R, RFn],
  val expectedToCode: (Either[Exception, R]) => CodeHolder[RFn],
  val buildChildEngine: BuildEngine[Params, BFn, R, RFn, R, E])(implicit val ldp: LoggerDisplayProcessor)
  extends BuildFoldingEngine[Params, BFn, R, RFn, FullR, F, E] {
  lazy val decisionTreeLens = new DecisionTreeLens[Params, BFn, R, RFn]
  lazy val evaluateTree = new SimpleEvaluateTree(makeClosures, decisionTreeLens, BuildEngine.validateScenario)
  lazy val blankTree = new SimpleDecisionTree[Params, BFn, R, RFn](root, rootIsDefault = true)
  lazy val builderWithModifyChildrenForBuild = new SimpleBuilderWithModifyChildrenForBuild[R, RFn]
}

