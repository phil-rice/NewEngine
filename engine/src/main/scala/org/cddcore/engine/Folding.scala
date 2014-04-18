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
  def trees: List[DecisionTree[Params, BFn, R, RFn]]
  def initialValue: CodeHolder[() => FullR]
  def foldingFn: (FullR, R) => FullR
  def applyParams(params: Params): FullR = {
    val result = trees.foldLeft(initialValue.fn())((acc, t) => foldingFn(acc, evaluator.evaluate(t, params)))
    result
  }
}
trait BuildFoldingEngine[Params, BFn, R, RFn, FullR, E <: Engine[Params, BFn, R, RFn]] extends BuildEngine[Params, BFn, R, RFn, FullR, E] {
  def constructEngine(
    requirement: Requirement,
    dts: List[DecisionTree[Params, BFn, R, RFn]],
    requirements: BuilderNodeHolder[R, RFn],
    exceptionMap: Map[BuilderNode[R, RFn], List[Exception]],
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR): E
    
  def buildEngine(requirements: BuilderNodeHolder[R, RFn],  buildExceptions: EMap) =
    requirements.nodes match {
      case (f: FoldingBuilderNodeAndHolder[R, RFn, FullR]) :: Nil => {
        val initial = (List[DT](), buildExceptions)
        if (f.nodes.isEmpty) throw new CannotHaveFoldingEngineWithoutChildEnginesException
        val (dts, eMap) = f.nodes.foldLeft(initial)((acc, ed) => ed match {
          case e: EngineDescription[R, RFn] => {
            val (dts, initialEMap) = acc
            val (dt, eMap) = buildTree(e, initialEMap)
            (dt :: dts, eMap)
          }
        });
        constructEngine(f, dts, requirements, eMap, f.initialValue, f.foldingFn)
      }
    }
}


abstract class SimpleFoldingBuildEngine[Params, BFn, R, RFn, FullR, E <: Engine[Params, BFn, R, RFn]](
  val root: DecisionTreeNode[Params, BFn, R, RFn],
  makeClosures: MakeClosures[Params, BFn, R, RFn],
  val expectedToCode: (Either[Exception, R]) => CodeHolder[RFn])(implicit val ldp: LoggerDisplayProcessor)
  extends BuildFoldingEngine[Params, BFn, R, RFn, FullR, E] {
  lazy val decisionTreeLens = new DecisionTreeLens[Params, BFn, R, RFn]
  lazy val evaluateTree = new SimpleEvaluateTree(makeClosures, decisionTreeLens)
  lazy val validator = new SimpleValidateScenario[Params, BFn, R, RFn]
  lazy val blankTree = new SimpleDecisionTree[Params, BFn, R, RFn](root, rootIsDefault = true)
  lazy val builderWithModifyChildrenForBuild = new SimpleBuilderWithModifyChildrenForBuild[R, RFn]
}

