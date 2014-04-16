package org.cddcore.engine
import org.cddcore.utilities.Maps
import sun.security.validator.SimpleValidator

object BuildEngine {
  def defaultRoot[Params, BFn, R, RFn](code: CodeHolder[RFn]) = Conclusion[Params, BFn, R, RFn](code, List())
  def defaultRootCode1[P, R]: CodeHolder[(P) => R] = new CodeHolder((p: P) => throw new UndecidedException, "throws Undecided Exception")
  def defaultRootCode2[P1, P2, R]: CodeHolder[(P1, P2) => R] = new CodeHolder((p1: P1, p2: P2) => throw new UndecidedException, "throws Undecided Exception")
  def defaultRootCode3[P1, P2, P3, R]: CodeHolder[(P1, P2, P3) => R] = new CodeHolder((p1: P1, p2: P2, p3: P3) => throw new UndecidedException, "throws Undecided Exception")

  def expectedToCode1[P, R]: Either[Exception, R] => CodeHolder[(P) => R] = (x) => new CodeHolder((p) => x match { case Right(r) => r }, x.toString())
  def expectedToCode2[P1, P2, R]: Either[Exception, R] => CodeHolder[(P1, P2) => R] = (x) => new CodeHolder((p1, p2) => x match { case Right(r) => r }, x.toString())
  def expectedToCode3[P1, P2, P3, R]: Either[Exception, R] => CodeHolder[(P1, P2, P3) => R] = (x) => new CodeHolder((p1, p2, p3) => x match { case Right(r) => r }, x.toString())

  //  def construct1[P, R, E] =  (dt: DecisionTree[P, (P) => Boolean, R, (P) => R ], 
  //      requirements: BuilderNodeHolder[R, (P)=> R], buildExceptions: Map[BuilderNode[R, RFn], List[Exception]]) => Engine1(requirements, buildExceptions)

  def builderEngine1[P, R, FullR] = new SimpleBuildEngine1[P, R, FullR]
  def builderEngine2[P1, P2, R, FullR] = new SimpleBuildEngine2[P1, P2, R, FullR]
  def builderEngine3[P1, P2, P3, R, FullR] = new SimpleBuildEngine3[P1, P2, P3, R, FullR]

}
case class SimpleBuildEngine1[P, R, FullR] extends SimpleBuildEngine[P, (P) => Boolean, R, (P) => R, FullR, Engine1[P, R]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode1), new MakeClosures1, BuildEngine.expectedToCode1) {
  def constructEngine(dt: DecisionTree[P, (P) => Boolean, R, (P) => R],
    requirements: BuilderNodeHolder[R, (P) => R],
    exceptionMap: Map[BuilderNode[R, (P) => R], List[Exception]]): Engine1[P, R] =
    Engine1(dt, evaluateTree, requirements, exceptionMap)

}
case class SimpleBuildEngine2[P1, P2, R, FullR] extends SimpleBuildEngine[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, FullR, Engine2[P1, P2, R]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode2), new MakeClosures2, BuildEngine.expectedToCode2) {
  def constructEngine(dt: DecisionTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R], requirements: BuilderNodeHolder[R, (P1, P2) => R], exceptionMap: Map[BuilderNode[R, (P1, P2) => R], List[Exception]]) =
    Engine2(dt, evaluateTree, requirements, exceptionMap)
}
case class SimpleBuildEngine3[P1, P2, P3, R, FullR] extends SimpleBuildEngine[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, FullR, Engine3[P1, P2, P3, R]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode3), new MakeClosures3, BuildEngine.expectedToCode3) {
  def constructEngine(dt: DecisionTree[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R], requirements: BuilderNodeHolder[R, (P1, P2, P3) => R], exceptionMap: Map[BuilderNode[R, (P1, P2, P3) => R], List[Exception]]) =
    Engine3(dt, evaluateTree, requirements, exceptionMap)
}

abstract class SimpleBuildEngine[Params, BFn, R, RFn, FullR, E <: Engine[Params, BFn, R, RFn]](
  val root: DecisionTreeNode[Params, BFn, R, RFn],
  makeClosures: MakeClosures[Params, BFn, R, RFn],
  val expectedToCode: (Either[Exception, R]) => CodeHolder[RFn])(implicit val ldp: LoggerDisplayProcessor) extends BuildEngine[Params, BFn, R, RFn, FullR, E] {
  lazy val decisionTreeLens = new DecisionTreeLens[Params, BFn, R, RFn]
  lazy val evaluateTree = new SimpleEvaluateTree(makeClosures, decisionTreeLens)
  lazy val validator = new SimpleValidateScenario[Params, BFn, R, RFn]
  lazy val blankTree = new SimpleDecisionTree[Params, BFn, R, RFn](root, rootIsDefault = true)
  lazy val builderWithModifyChildrenForBuild = new SimpleBuilderWithModifyChildrenForBuild[R, RFn]
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
  def constructEngine(dt: DecisionTree[Params, BFn, R, RFn], requirements: BuilderNodeHolder[R, RFn], exceptionMap: Map[BuilderNode[R, RFn], List[Exception]]): E
  val mc = evaluateTree.makeClosures
  implicit def ldp: LoggerDisplayProcessor

  def buildEngine(requirements: BuilderNodeHolder[R, RFn], buildExceptions: EMap) = {
    val (dt, eMap) = buildTree(requirements, buildExceptions)
    constructEngine(dt, requirements, eMap)
  }

  def buildTree(requirements: BuilderNodeHolder[R, RFn], buildExceptions: EMap): (DT, EMap) = {
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

  def addScenario(tree: DT, scenario: S): DT = {
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



