package org.cddcore.engine
import org.cddcore.utilities.Maps
import sun.security.validator.SimpleValidator

object BuildEngine {
  def defaultRoot[Params, BFn, R, RFn](code: CodeHolder[RFn]) = Conclusion[Params, BFn, R, RFn](code, List())
  def defaultRootCode1[P, R]: CodeHolder[(P) => R] = new CodeHolder((p: P) => throw new UndecidedException, "throws Undecided Exception")
  def defaultRootCode2[P1, P2, R]: CodeHolder[(P1, P2) => R] = new CodeHolder((p1: P1, p2: P2) => throw new UndecidedException, "throws Undecided Exception")
  def defaultRootCode3[P1, P2, P3, R]: CodeHolder[(P1, P2, P3) => R] = new CodeHolder((p1: P1, p2: P2, p3: P3) => throw new UndecidedException, "throws Undecided Exception")

  def expectedToCode1[P, R]: Either[Exception, R] => CodeHolder[(P) => R] = (x) => new CodeHolder((p) => x match { case Right(r) => r }, x.toString())

  def apply[P, R, FullR] =
    new SimpleBuildEngine[P, (P) => Boolean, R, (P) => R, FullR, Engine1[P, R]](defaultRoot(defaultRootCode1), new MakeClosures1)
  def apply[P1, P2, R, FullR] =
    new SimpleBuildEngine[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, FullR, Engine2[P1, P2, R]](defaultRoot(defaultRootCode2), new MakeClosures2)
  def apply[P1, P2, P3, R, FullR] =
    new SimpleBuildEngine[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, FullR, Engine3[P1, P2, P3, R]](defaultRoot(defaultRootCode3), new MakeClosures3)

}

class SimpleBuildEngine[Params, BFn, R, RFn, FullR, E <: Engine[Params, BFn, R, RFn]](
  root: DecisionTreeNode[Params, BFn, R, RFn],
  makeClosures: MakeClosures[Params, BFn, R, RFn])(implicit val ldp: LoggerDisplayProcessor) extends BuildEngine[Params, BFn, R, RFn, FullR, E] {
  val evaluateTree = new SimpleEvaluateTree(makeClosures, new DecisionTreeLens[Params, BFn, R, RFn])
  val validator = new SimpleValidateScenario[Params, BFn, R, RFn]
  val blankTree = new SimpleDecisionTree[Params, BFn, R, RFn](root, rootIsDefault = true)
  val requirementsModifier = new SimpleBuilderWithModifyChildrenForBuild[R, RFn]
  val expectedToCode: (Either[Exception, R]) => CodeHolder[RFn]
}

trait BuildEngine[Params, BFn, R, RFn, FullR, E <: Engine[Params, BFn, R, RFn]] {
  type DT = DecisionTree[Params, BFn, R, RFn]
  type DN = DecisionTreeNode[Params, BFn, R, RFn]
  type Dec = Decision[Params, BFn, R, RFn]
  type Conc = Conclusion[Params, BFn, R, RFn]
  type S = Scenario[Params, BFn, R, RFn]
  type EMap = Map[BuilderNode[R, RFn], List[Exception]]

  val evaluateTree: EvaluateTree[Params, BFn, R, RFn]
  val validator: ValidateScenario[Params, BFn, R, RFn]
  val blankTree: DT
  val requirementsModifier: BuilderWithModifyChildrenForBuild[R, RFn]
  def expectedToCode: (Either[Exception, R]) => CodeHolder[RFn]
  val makeClosures = evaluateTree.makeClosures
  implicit def ldp: LoggerDisplayProcessor

  def buildTree(requirements: BuilderNodeHolder[R, RFn], buildExceptions: EMap): (DT, EMap) = {
    val scenarios = requirements.all(classOf[S])
    val (newDecisionTree, newENap) = scenarios.foldLeft((blankTree, buildExceptions))((acc, s) => {
      val (dt, eMap) = acc
      try {
        validator.preValidateScenario(makeClosures, s)
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
    import evaluateTree.lens._
    def actualFromNewScenario(c: Conc) = makeClosures.safeEvaluateResult(c.code.fn, scenario)

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
          c.scenarios.filter((s) => makeClosures.evaluateBecause(scenario, s)) match {
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



