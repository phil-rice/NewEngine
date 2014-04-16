package org.cddcore.engine
import org.cddcore.utilities.Maps
object BuildEngine {

  implicit val ldp = SimpleLoggerDisplayProcessor()
  def defaultRoot[Params, BFn, R, RFn](code: CodeHolder[RFn]) =
    Conclusion[Params, BFn, R, RFn](code, List())
  def defaultRootCode1[P, R]: CodeHolder[(P) => R] =
    new CodeHolder((p: P) => throw new UndecidedException, "throws Undecided Exception")
  def defaultRootCode2[P1, P2, R]: CodeHolder[(P1, P2) => R] =
    new CodeHolder((p1: P1, p2: P2) => throw new UndecidedException, "throws Undecided Exception")
  def defaultRootCode3[P1, P2, P3, R]: CodeHolder[(P1, P2, P3) => R] =
    new CodeHolder((p1: P1, p2: P2, p3: P3) => throw new UndecidedException, "throws Undecided Exception")

  private def build[Params, BFn, R, RFn, B <: Builder[R, RFn, B]](builder: Builder[R, RFn, B], dt: DecisionTreeAndExceptions[Params, BFn, R, RFn], validator: ValidateScenario[Params, BFn, R, RFn]) = {
    val modifiedChildren = builder.asInstanceOf[BuilderWithModifyChildrenForBuild[R, RFn]].modifyChildrenForBuild
    val scenarios = modifiedChildren.all(classOf[Scenario[Params, BFn, R, RFn]])
    val newDecisionTree = scenarios.foldLeft(dt)((dt, s) =>
      try {
        validator.preValidateScenario(s)
        val newTree = addScenario(builder, dt, s)
        validator.checkAssertions(newTree, s)
        newTree
      } catch {
        case e: Exception =>
          Engine.testing match {
            case false => throw e;
            case true => 
              val result = dt.lens.exceptionMapL(builder).set(dt, Maps.addToList(dt.buildExceptions, s, e))
              result
          }
      })
    if (!Engine.testing)
      scenarios.foreach((x) => validator.postValidateScenario(newDecisionTree, x))
    newDecisionTree
  }

  def build1[P, R, FullR](builder: Builder1[P, R, FullR]) =
    build(builder,
      new Engine1(defaultRoot[P, (P) => Boolean, R, (P) => R](defaultRootCode1[P, R]), builder, builder.buildExceptions, rootIsDefault = true),
      builder).asInstanceOf[Engine1[P, R]]
  def build2[P1, P2, R](builder: Builder2[P1, P2, R]) =
    build(builder,
      new Engine2(defaultRoot[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R](defaultRootCode2[P1, P2, R]), builder, builder.buildExceptions, rootIsDefault = true),
      builder).asInstanceOf[Engine2[P1, P2, R]]
  def build3[P1, P2, P3, R](builder: Builder3[P1, P2, P3, R]) =
    build(builder,
      new Engine3(defaultRoot[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R](defaultRootCode3[P1, P2, P3, R]), builder, builder.buildExceptions, rootIsDefault = true),
      builder).asInstanceOf[Engine3[P1, P2, P3, R]]

  def addScenario[Params, BFn, R, RFn](requirements: BuilderNodeHolder[R, RFn], tree: DecisionTreeAndExceptions[Params, BFn, R, RFn], scenario: Scenario[Params, BFn, R, RFn])(implicit ldp: LoggerDisplayProcessor): DecisionTreeAndExceptions[Params, BFn, R, RFn] = {
    import tree.lens._
    type DT = DecisionTreeAndExceptions[Params, BFn, R, RFn]
    type DN = DecisionTreeNode[Params, BFn, R, RFn]
    type Dec = Decision[Params, BFn, R, RFn]
    type Conc = Conclusion[Params, BFn, R, RFn]
    def actualFromNewScenario(c: Conc) = tree.safeEvaluateResult(c.code.fn, scenario)

    val concL = tree.findLensToConclusion(requirements, tree.buildExceptions, tree.makeBecauseClosure(scenario))
    val concLToConclusionL = concL.andThen(toConclusionL)
    val oldConclusion = concLToConclusionL.get(tree)
    val actual = actualFromNewScenario(oldConclusion)
    val expected = scenario.expected.get
    val comesToSameConclusion = Reportable.compare(actual, expected)
    def newConclusion = Conclusion(code = scenario.actualCode(tree.expectedToCode), List(scenario))
    def addAssertion(lensToNode: Lens[DT, Conc]) = lensToNode.mod(tree, (c) => c.copy(scenarios = c.scenarios :+ scenario))
    def addDecisionNodeTo = {
      concL.mod(tree, (c) => c match {
        case c: Conc =>
          c.scenarios.filter((s) => tree.evaluateBecause(scenario, s)) match {
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

