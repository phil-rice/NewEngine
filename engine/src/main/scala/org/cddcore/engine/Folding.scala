package org.cddcore.engine

trait FoldingBuilderNodeAndHolder[R, RFn, FullR] extends BuilderNodeAndHolder[R, RFn] {
  def foldingFn: (FullR, R) => FullR
  def initialValue: CodeHolder[() => FullR]
}
case class FoldingEngineDescription[R, RFn, FullR](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[BuilderNode[R, RFn]] = List(),
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val foldingFn: (FullR, R) => FullR,
  val initialValue: CodeHolder[() => FullR])
  extends BuilderNodeAndHolder[R, RFn] with FoldingBuilderNodeAndHolder[R, RFn, FullR] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new FoldingEngineDescription[R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new FoldingEngineDescription[R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  def copyNodes(nodes: List[BuilderNode[R, RFn]]) =
    new FoldingEngineDescription[R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  override def toString = s"FoldingEngineDescription(${initialValue.description}, $foldingFn, nodes=${nodes.mkString(", ")}"

}

trait FoldingEngine[Params, BFn, R, RFn, FullR] extends HasExceptionMap[R, RFn] with Engine[Params, BFn, R, RFn] {
  def engines: List[EngineFromTests[Params, BFn, R, RFn]]
  def initialValue: CodeHolder[() => FullR]
  def foldingFn: (FullR, R) => FullR
  def applyParams(params: Params): FullR = {
    val result = engines.foldLeft(initialValue.fn())((acc, e) => foldingFn(acc, e.applyParams(params)))
    result
  }
}
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

