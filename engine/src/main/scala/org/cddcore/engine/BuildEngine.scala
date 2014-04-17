package org.cddcore.engine
import org.cddcore.utilities.Maps
import sun.security.validator.SimpleValidator

object BuildEngine {
  def initialNodes[R, RFn] = List(EngineDescription[R, RFn]())
  def initialNodes[Params, BFn, R, RFn, FullR](initialValue: FullR, foldingFn: (FullR, R) => FullR) =
    List(FoldingEngineDescription[R, RFn, FullR](initialValue = new CodeHolder(() => initialValue, initialValue.toString), foldingFn = foldingFn))

  def defaultRoot[Params, BFn, R, RFn](code: CodeHolder[RFn]) = Conclusion[Params, BFn, R, RFn](code, List())
  def defaultRootCode1[P, R]: CodeHolder[(P) => R] = new CodeHolder((p: P) => throw new UndecidedException, "throws Undecided Exception")
  def defaultRootCode2[P1, P2, R]: CodeHolder[(P1, P2) => R] = new CodeHolder((p1: P1, p2: P2) => throw new UndecidedException, "throws Undecided Exception")
  def defaultRootCode3[P1, P2, P3, R]: CodeHolder[(P1, P2, P3) => R] = new CodeHolder((p1: P1, p2: P2, p3: P3) => throw new UndecidedException, "throws Undecided Exception")

  def expectedToCode1[P, R]: Either[Exception, R] => CodeHolder[(P) => R] = (x) => new CodeHolder((p) => x match { case Right(r) => r }, x.toString())
  def expectedToCode2[P1, P2, R]: Either[Exception, R] => CodeHolder[(P1, P2) => R] = (x) => new CodeHolder((p1, p2) => x match { case Right(r) => r }, x.toString())
  def expectedToCode3[P1, P2, P3, R]: Either[Exception, R] => CodeHolder[(P1, P2, P3) => R] = (x) => new CodeHolder((p1, p2, p3) => x match { case Right(r) => r }, x.toString())

  //  def construct1[P, R, E] =  (dt: DecisionTree[P, (P) => Boolean, R, (P) => R ], 
  //      requirements: BuilderNodeHolder[R, (P)=> R], buildExceptions: Map[BuilderNode[R, RFn], List[Exception]]) => Engine1(requirements, buildExceptions)

  def builderEngine1[P, R] = new SimpleBuildEngine1[P, R]
  def builderEngine2[P1, P2, R] = new SimpleBuildEngine2[P1, P2, R]
  def builderEngine3[P1, P2, P3, R] = new SimpleBuildEngine3[P1, P2, P3, R]
  def folderBuilderEngine1[P, R, FullR] = new FoldingBuildEngine1[P, R, FullR]
  def folderBuilderEngine2[P1, P2, R, FullR] = new FoldingBuildEngine2[P1, P2, R, FullR]
  def folderBuilderEngine3[P1, P2, P3, R, FullR] = new FoldingBuildEngine3[P1, P2, P3, R, FullR]

}
case class SimpleBuildEngine1[P, R] extends SimpleBuildEngine[P, (P) => Boolean, R, (P) => R, Engine1[P, R, R]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode1), new MakeClosures1, BuildEngine.expectedToCode1) {
  def constructEngine(dt: DecisionTree[P, (P) => Boolean, R, (P) => R],
    requirements: BuilderNodeHolder[R, (P) => R],
    exceptionMap: Map[BuilderNode[R, (P) => R], List[Exception]]): Engine1[P, R, R] =
    Engine1FromTests(dt, evaluateTree, requirements, exceptionMap)

}
class FoldingBuildEngine1[P, R, FullR] extends SimpleFoldingBuildEngine[P, (P) => Boolean, R, (P) => R, FullR, Engine1[P, R, FullR]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode1), new MakeClosures1, BuildEngine.expectedToCode1) {
  def constructEngine(
    dts: List[DecisionTree[P, (P) => Boolean, R, (P) => R]],
    requirements: BuilderNodeHolder[R, (P) => R],
    exceptionMap: Map[BuilderNode[R, (P) => R], List[Exception]],
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR): FoldingEngine1[P, R, FullR] =
    FoldingEngine1(dts, evaluateTree, requirements, exceptionMap, initialValue, foldingFn)
}
class FoldingBuildEngine2[P1, P2, R, FullR] extends SimpleFoldingBuildEngine[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, FullR, Engine2[P1, P2, R, FullR]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode2), new MakeClosures2, BuildEngine.expectedToCode2) {
  def constructEngine(
    dts: List[DecisionTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]],
    requirements: BuilderNodeHolder[R, (P1, P2) => R],
    exceptionMap: Map[BuilderNode[R, (P1, P2) => R], List[Exception]],
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR): FoldingEngine2[P1, P2, R, FullR] =
    FoldingEngine2(dts, evaluateTree, requirements, exceptionMap, initialValue, foldingFn)
}
class FoldingBuildEngine3[P1, P2, P3, R, FullR] extends SimpleFoldingBuildEngine[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, FullR, Engine3[P1, P2, P3, R, FullR]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode3), new MakeClosures3, BuildEngine.expectedToCode3) {
  def constructEngine(
    dts: List[DecisionTree[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]],
    requirements: BuilderNodeHolder[R, (P1, P2, P3) => R],
    exceptionMap: Map[BuilderNode[R, (P1, P2, P3) => R], List[Exception]],
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR): FoldingEngine3[P1, P2, P3, R, FullR] =
    FoldingEngine3(dts, evaluateTree, requirements, exceptionMap, initialValue, foldingFn)
}
case class SimpleBuildEngine2[P1, P2, R] extends SimpleBuildEngine[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Engine2[P1, P2, R, R]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode2), new MakeClosures2, BuildEngine.expectedToCode2) {
  def constructEngine(dt: DecisionTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R], requirements: BuilderNodeHolder[R, (P1, P2) => R], exceptionMap: Map[BuilderNode[R, (P1, P2) => R], List[Exception]]) =
    Engine2FromTests(dt, evaluateTree, requirements, exceptionMap)
}
case class SimpleBuildEngine3[P1, P2, P3, R] extends SimpleBuildEngine[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, Engine3[P1, P2, P3, R, R]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode3), new MakeClosures3, BuildEngine.expectedToCode3) {
  def constructEngine(dt: DecisionTree[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R], requirements: BuilderNodeHolder[R, (P1, P2, P3) => R], exceptionMap: Map[BuilderNode[R, (P1, P2, P3) => R], List[Exception]]) =
    Engine3FromTests(dt, evaluateTree, requirements, exceptionMap)
}

abstract class SimpleFoldingBuildEngine[Params, BFn, R, RFn, FullR, E <: Engine[Params, BFn, R, RFn]](
  val root: DecisionTreeNode[Params, BFn, R, RFn],
  makeClosures: MakeClosures[Params, BFn, R, RFn],
  val expectedToCode: (Either[Exception, R]) => CodeHolder[RFn])(implicit val ldp: LoggerDisplayProcessor) extends BuildFoldingEngine[Params, BFn, R, RFn, FullR, E] {
  lazy val decisionTreeLens = new DecisionTreeLens[Params, BFn, R, RFn]
  lazy val evaluateTree = new SimpleEvaluateTree(makeClosures, decisionTreeLens)
  lazy val validator = new SimpleValidateScenario[Params, BFn, R, RFn]
  lazy val blankTree = new SimpleDecisionTree[Params, BFn, R, RFn](root, rootIsDefault = true)
  lazy val builderWithModifyChildrenForBuild = new SimpleBuilderWithModifyChildrenForBuild[R, RFn]
}

abstract class SimpleBuildEngine[Params, BFn, R, RFn, E <: Engine[Params, BFn, R, RFn]](
  val root: DecisionTreeNode[Params, BFn, R, RFn],
  makeClosures: MakeClosures[Params, BFn, R, RFn],
  val expectedToCode: (Either[Exception, R]) => CodeHolder[RFn])(implicit val ldp: LoggerDisplayProcessor) extends BuildEngineFromTests[Params, BFn, R, RFn, E] {
  lazy val decisionTreeLens = new DecisionTreeLens[Params, BFn, R, RFn]
  lazy val evaluateTree = new SimpleEvaluateTree(makeClosures, decisionTreeLens)
  lazy val validator = new SimpleValidateScenario[Params, BFn, R, RFn]
  lazy val blankTree = new SimpleDecisionTree[Params, BFn, R, RFn](root, rootIsDefault = true)
  lazy val builderWithModifyChildrenForBuild = new SimpleBuilderWithModifyChildrenForBuild[R, RFn]
}

trait BuildEngineFromTests[Params, BFn, R, RFn, E <: Engine[Params, BFn, R, RFn]] extends BuildEngine[Params, BFn, R, RFn, R, E] {
  def constructEngine(dt: DecisionTree[Params, BFn, R, RFn], requirements: BuilderNodeHolder[R, RFn], exceptionMap: Map[BuilderNode[R, RFn], List[Exception]]): E
  def buildEngine(requirements: BuilderNodeHolder[R, RFn], buildExceptions: EMap) = {
    val (dt, eMap) = buildTree(requirements, buildExceptions)
    constructEngine(dt, requirements, eMap)
  }
}

trait BuildFoldingEngine[Params, BFn, R, RFn, FullR, E <: Engine[Params, BFn, R, RFn]] extends BuildEngine[Params, BFn, R, RFn, FullR, E] {
  def constructEngine(dts: List[DecisionTree[Params, BFn, R, RFn]],
    requirements: BuilderNodeHolder[R, RFn],
    exceptionMap: Map[BuilderNode[R, RFn], List[Exception]],
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR): E
  def buildEngine(requirements: BuilderNodeHolder[R, RFn], buildExceptions: EMap) =
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
        constructEngine(dts, requirements, eMap, f.initialValue, f.foldingFn)
      }
    }
}
trait BuildEngine[Params, BFn, R, RFn, FullR, E <: Engine[Params, BFn, R, RFn]] {
  type DT = DecisionTree[Params, BFn, R, RFn]
  type DN = DecisionTreeNode[Params, BFn, R, RFn]
  type Dec = Decision[Params, BFn, R, RFn]
  type Conc = Conclusion[Params, BFn, R, RFn]
  type S = Scenario[Params, BFn, R, RFn]
  type EMap = Map[BuilderNode[R, RFn], List[Exception]]

  def evaluateTree: EvaluateTree[Params, BFn, R, RFn]
  def validator: ValidateScenario[Params, BFn, R, RFn]
  def blankTree: DT
  def builderWithModifyChildrenForBuild: BuilderWithModifyChildrenForBuild[R, RFn]
  def expectedToCode: (Either[Exception, R]) => CodeHolder[RFn]
  def decisionTreeLens: DecisionTreeLens[Params, BFn, R, RFn]
  val mc = evaluateTree.makeClosures
  implicit def ldp: LoggerDisplayProcessor

  def buildEngine(requirements: BuilderNodeHolder[R, RFn], buildExceptions: EMap): E

  protected def buildTree(requirements: BuilderNodeHolder[R, RFn], buildExceptions: EMap): (DT, EMap) = {
    val scenarios = builderWithModifyChildrenForBuild.modifyChildrenForBuild(requirements).all(classOf[S])
    val (newDecisionTree, newENap) = scenarios.foldLeft((blankTree, buildExceptions))((acc, s) => {
      val (dt, eMap) = acc
      try {
        validator.preValidateScenario(mc, s)
        val newTree = addScenario(dt, s)
        validator.checkAssertions(evaluateTree, newTree, s)
        (newTree, eMap)
      } catch {
        case e: Exception =>
          Engine.testing match {
            case false => throw e;
            case true => (dt, Maps.addToList(eMap, s, e))
          }
      }
    })
    if (!Engine.testing)
      scenarios.foreach((x) => validator.postValidateScenario(evaluateTree, newDecisionTree, x))
    (newDecisionTree, newENap)
  }
  protected def addScenario(tree: DT, scenario: S): DT = {
    val lens = evaluateTree.lens
    import lens._
    def actualFromNewScenario(c: Conc) = mc.safeEvaluateResult(c.code.fn, scenario)

    val concL = evaluateTree.findLensToConclusion(tree.root, scenario)
    val concLToConclusionL = concL.andThen(toConclusionL)
    val oldConclusion = concLToConclusionL.get(tree)
    val actual = actualFromNewScenario(oldConclusion)
    val expected = scenario.expected.get
    val comesToSameConclusion = Reportable.compare(actual, expected)
    def newConclusion = Conclusion(code = scenario.actualCode(expectedToCode), List(scenario))
    def addAssertion(lensToNode: Lens[DT, Conc]) = lensToNode.mod(tree, (c) => c.copy(scenarios = c.scenarios :+ scenario))
    def addDecisionNodeTo = {
      concL.mod(tree, (c) => c match {
        case c: Conc =>
          c.scenarios.filter((s) => mc.evaluateBecause(scenario, s)) match {
            case Nil => Decision(List(scenario.because.get), yes = newConclusion, no = c, scenarioThatCausedNode = scenario)
            case brokenScenarios => throw ScenarioConflictAndBecauseNotAdequateException(concL, expected, actual, brokenScenarios, scenario)
          }
        case _ => throw new IllegalStateException
      })
    }

    val result: DT =
      (comesToSameConclusion, scenario.because, tree.rootIsDefault) match {
        case (false, None, true) => concLToConclusionL.set(tree, newConclusion)
        case (true, _, _) => addAssertion(concL.andThen(toConclusionL)) //latter on this will be the "or rule" place 
        case (false, None, _) =>
          oldConclusion.scenarios match {
            case Nil => throw ScenarioConflictingWithDefaultAndNoBecauseException(concL, actual, expected, scenario)
            case existing => throw ScenarioConflictingWithoutBecauseException(concL, actual, expected, existing, scenario)
          }
        case (false, Some(b), _) => addDecisionNodeTo
      }
    result
  }
}



