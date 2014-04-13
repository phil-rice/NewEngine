package org.cddcore.engine

object BuildEngine {
  def defaultRoot[Params, BFn, R, RFn](code: CodeHolder[RFn]) =
    Conclusion[Params, BFn, R, RFn](List(), code)
  //  def rootFor[Params, BFn, R, RFn](ed: EngineDescription[R, RFn]) = defaultRoot[Params, BFn, R, RFn]
  def addScenario[Params, BFn, R, RFn](root: DecisionTreeNode[Params, BFn, R, RFn], s: Scenario[Params, BFn, R, RFn]): DecisionTreeNode[Params, BFn, R, RFn] = {
    root
  }
  def defaultRootCode1[P, R]: CodeHolder[(P) => R] =
    new CodeHolder((p: P) => throw new UndecidedException, "throws Undecided Exception")
  def defaultRootCode2[P1, P2, R]: CodeHolder[(P1, P2) => R] =
    new CodeHolder((p1: P1, p2: P2) => throw new UndecidedException, "throws Undecided Exception")
  def defaultRootCode3[P1, P2, P3, R]: CodeHolder[(P1, P2, P3) => R] =
    new CodeHolder((p1: P1, p2: P2, p3: P3) => throw new UndecidedException, "throws Undecided Exception")

  private def build[Params, BFn, R, RFn, B <: Builder[R, RFn, B]](builder: Builder[R, RFn, B], dt: DecisionTree[Params, BFn, R, RFn]) = dt
  def build1[P, R](builder: Builder1[P, R]) =
    build(builder, new Engine1(defaultRoot[P, (P) => Boolean, R, (P) => R](defaultRootCode1[P, R]))).asInstanceOf[Engine1[P, R]]
  def build2[P1, P2, R](builder: Builder2[P1, P2, R]) =
    build(builder, new Engine2(defaultRoot[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R](defaultRootCode2[P1, P2, R]))).asInstanceOf[Engine2[P1, P2, R]]
  def build3[P1, P2, P3, R](builder: Builder3[P1, P2, P3, R]) =
    build(builder, new Engine3(defaultRoot[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R](defaultRootCode3[P1, P2, P3, R]))).asInstanceOf[Engine3[P1, P2, P3, R]]

}

 
