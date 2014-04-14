package org.cddcore.engine

object BuildEngine {
  def defaultRoot[Params, BFn, R, RFn](code: CodeHolder[RFn]) =
    Conclusion[Params, BFn, R, RFn](code, List())
  //  def rootFor[Params, BFn, R, RFn](ed: EngineDescription[R, RFn]) = defaultRoot[Params, BFn, R, RFn]
  def defaultRootCode1[P, R]: CodeHolder[(P) => R] =
    new CodeHolder((p: P) => throw new UndecidedException, "throws Undecided Exception")
  def defaultRootCode2[P1, P2, R]: CodeHolder[(P1, P2) => R] =
    new CodeHolder((p1: P1, p2: P2) => throw new UndecidedException, "throws Undecided Exception")
  def defaultRootCode3[P1, P2, P3, R]: CodeHolder[(P1, P2, P3) => R] =
    new CodeHolder((p1: P1, p2: P2, p3: P3) => throw new UndecidedException, "throws Undecided Exception")

  private def build[Params, BFn, R, RFn, B <: Builder[R, RFn, B]](builder: Builder[R, RFn, B], dt: DecisionTree[Params, BFn, R, RFn]) = {
    val modifiedChildren = builder.asInstanceOf[BuilderWithModifyChildrenForBuild[R, RFn]].modifyChildrenForBuild
    val scenarios = modifiedChildren.all(classOf[Scenario[Params, BFn, R, RFn]])
    val newDecisionTree = scenarios.foldLeft(dt)((dt, s) => addScenario(dt, s))
    newDecisionTree
  }

  def build1[P, R](builder: Builder1[P, R]) =
    build(builder, new Engine1(defaultRoot[P, (P) => Boolean, R, (P) => R](defaultRootCode1[P, R]), rootIsDefault = true)).asInstanceOf[Engine1[P, R]]
  def build2[P1, P2, R](builder: Builder2[P1, P2, R]) =
    build(builder, new Engine2(defaultRoot[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R](defaultRootCode2[P1, P2, R]), rootIsDefault = true)).asInstanceOf[Engine2[P1, P2, R]]
  def build3[P1, P2, P3, R](builder: Builder3[P1, P2, P3, R]) =
    build(builder, new Engine3(defaultRoot[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R](defaultRootCode3[P1, P2, P3, R]), rootIsDefault = true)).asInstanceOf[Engine3[P1, P2, P3, R]]

  def addScenario[Params, BFn, R, RFn](tree: DecisionTree[Params, BFn, R, RFn], s: Scenario[Params, BFn, R, RFn]): DecisionTree[Params, BFn, R, RFn] = {
    val bc = tree.makeBecauseClosure(s.params)
    val newConclusion = Conclusion( code = s.actualCode(tree.expectedToCode), List(s))
    val newRoot = (tree.rootIsDefault, tree.findParentLens(bc)) match {
      case (true, None) =>
        newConclusion
      case (false, None) =>
        Decision(List(s.because.get), yes = newConclusion, no=tree.root,  scenarioThatCausedNode = s)
      case (_, Some((parentLens, wasTrue))) =>
        tree.root
    }
    val result = tree.lens.creator(newRoot)
    result
  }
}

 
