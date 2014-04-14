package org.cddcore.engine

object BuildEngine {

  def checkAllScenarios[Params, BFn, R, RFn](tree: DecisionTree[Params, BFn, R, RFn], scenarios: Scenario[Params, BFn, R, RFn]*) {
    for (s <- scenarios) {
      val actual = tree.safeEvaluate(s.params)
      s.expected match {
        case Some(ex) => if (!Reportable.compare(ex, actual)) throw CameToWrongConclusionScenarioException(ex, actual, s)
        case _ => throw NoExpectedException(s)
      }
    }
  }

  implicit val ldp = SimpleLoggerDisplayProcessor()
  def defaultRoot[Params, BFn, R, RFn](code: CodeHolder[RFn]) =
    Conclusion[Params, BFn, R, RFn](code, List())
  def defaultRootCode1[P, R]: CodeHolder[(P) => R] =
    new CodeHolder((p: P) => throw new UndecidedException, "throws Undecided Exception")
  def defaultRootCode2[P1, P2, R]: CodeHolder[(P1, P2) => R] =
    new CodeHolder((p1: P1, p2: P2) => throw new UndecidedException, "throws Undecided Exception")
  def defaultRootCode3[P1, P2, P3, R]: CodeHolder[(P1, P2, P3) => R] =
    new CodeHolder((p1: P1, p2: P2, p3: P3) => throw new UndecidedException, "throws Undecided Exception")

  private def build[Params, BFn, R, RFn, B <: Builder[R, RFn, B]](builder: Builder[R, RFn, B], dt: DecisionTree[Params, BFn, R, RFn], validator: ValidateScenario[Params, BFn, R, RFn]) = {
    val modifiedChildren = builder.asInstanceOf[BuilderWithModifyChildrenForBuild[R, RFn]].modifyChildrenForBuild
    val scenarios = modifiedChildren.all(classOf[Scenario[Params, BFn, R, RFn]])
    scenarios.foreach((x) => validator.validateScenario(x))
    val newDecisionTree = scenarios.foldLeft(dt)((dt, s) => addScenario(builder, dt, s))
    checkAllScenarios(newDecisionTree, scenarios.toSeq: _*)
    newDecisionTree
  }

  def build1[P, R](builder: Builder1[P, R]) =
    build(builder, new Engine1(defaultRoot[P, (P) => Boolean, R, (P) => R](defaultRootCode1[P, R]), builder, rootIsDefault = true), builder).asInstanceOf[Engine1[P, R]]
  def build2[P1, P2, R](builder: Builder2[P1, P2, R]) =
    build(builder, new Engine2(defaultRoot[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R](defaultRootCode2[P1, P2, R]), builder, rootIsDefault = true), builder).asInstanceOf[Engine2[P1, P2, R]]
  def build3[P1, P2, P3, R](builder: Builder3[P1, P2, P3, R]) =
    build(builder, new Engine3(defaultRoot[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R](defaultRootCode3[P1, P2, P3, R]), builder, rootIsDefault = true), builder).asInstanceOf[Engine3[P1, P2, P3, R]]

  def addScenario[Params, BFn, R, RFn](requirements: EngineNodeHolder[R, RFn], tree: DecisionTree[Params, BFn, R, RFn], s: Scenario[Params, BFn, R, RFn])(implicit ldp: LoggerDisplayProcessor): DecisionTree[Params, BFn, R, RFn] = {
    import tree.lens._
    type DT = DecisionTree[Params, BFn, R, RFn]
    type DN = DecisionTreeNode[Params, BFn, R, RFn]
    type Dec = Decision[Params, BFn, R, RFn]
    type Conc = Conclusion[Params, BFn, R, RFn]

    def actualFromNewScenario(c: Conc) = tree.safeEvaluteResult(c.code.fn, s.params)
    def newConclusion = Conclusion(code = s.actualCode(tree.expectedToCode), List(s))
    def addAssertion(lensToNode: Lens[DT, Conc]) = lensToNode.mod(tree, (c) => {
      val actual = actualFromNewScenario(c)
      if (actual != s.expected.get)
        throw ScenarioConflictingWithDefaultException[R](actual, s)
      c.copy(scenarios = c.scenarios :+ s)
    })
    def addTo(lensToNode: Lens[DT, DN]) = {
      lensToNode.mod(tree, (c) => c match {
        case c: Conc =>
          def dnAsNo = Decision(List(s.because.get), yes = newConclusion, no = c, scenarioThatCausedNode = s)
          dnAsNo
      })
    }

    val result =
      if (tree.rootIsDefault)
        if (s.because.isDefined)
          addTo(rootL(requirements))
        else
          rootL(requirements).set(tree, newConclusion)
      else {
        val concL = tree.findLensToConclusion(requirements, tree.makeBecauseClosure(s.params))
        s.because match {
          case Some(b) => addTo(concL)
          case _ =>
            addAssertion(concL.andThen(toConclusionL))
        }
      }
    result
  }
}

