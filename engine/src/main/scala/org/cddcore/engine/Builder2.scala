package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros

object Builder2 {
  def bl[P1, P2, R, FullR]() = new FullBuilderLens[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, FullR, Builder2[P1, P2, R, FullR]]
  def becauseImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(because: c.Expr[(P1, P2) => Boolean]): c.Expr[Builder2[P1, P2, R, FullR]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, R, FullR]()
      val ch = CodeHolder[(P1, P2) => Boolean](because.splice, c.literal(show(because.tree)).splice)
      val thisObject: Builder2[P1, P2, R, FullR] = (c.Expr[Builder2[P1, P2, R, FullR]](c.prefix.tree)).splice
      thisObject.becauseHolder(ch)
    }
  }
  def codeImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(code: c.Expr[(P1, P2) => R]): c.Expr[Builder2[P1, P2, R, FullR]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, R, FullR]()
      val ch = CodeHolder[(P1, P2) => R](code.splice, c.literal(show(code.tree)).splice)
      val thisObject: Builder2[P1, P2, R, FullR] = (c.Expr[Builder2[P1, P2, R, FullR]](c.prefix.tree)).splice
      thisObject.codeHolder(ch)
    }
  }
  def matchWithImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(pf: c.Expr[PartialFunction[(P1, P2), R]]): c.Expr[Builder2[P1, P2, R, FullR]] = {
    import c.universe._
    val thisObject: Builder2[P1, P2, R, FullR] = (c.Expr[Builder2[P1, P2, R, FullR]](c.prefix.tree)).splice
    reify {
      val l = bl[P1, P2, R, FullR]()
      val chBecause = CodeHolder[(P1, P2) => Boolean]((p1, p2) => pf.splice.isDefinedAt((p1, p2)), c.literal(show(pf.tree)).splice)
      val chResult = CodeHolder[(P1, P2) => R]((p1, p2) => pf.splice.apply((p1, p2)), c.literal(show(pf.tree)).splice)

      thisObject.becauseHolder(chBecause).codeHolder(chResult)
    }
  }
}

case class Builder2[P1, P2, R, FullR](
  nodes: List[BuilderNode[R, (P1, P2) => R]] = List(new EngineDescription[R, (P1, P2) => R]),
  buildExceptions: Map[BuilderNode[R, (P1, P2) => R], List[Exception]] = Map[BuilderNode[R, (P1, P2) => R], List[Exception]](),
  buildEngine: BuildEngine[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, FullR, Engine2[P1, P2, R, FullR]])(implicit val ldp: LoggerDisplayProcessor)
  extends Builder[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, FullR, Builder2[P1, P2, R, FullR], Engine2[P1, P2, R, FullR]] {

  val makeClosures = buildEngine.mc
  val validator = buildEngine.validator
  import bl._
  import makeClosures._
  import validator._

  def code(code: (P1, P2) => R): Builder2[P1, P2, R, FullR] = macro Builder2.codeImpl[P1, P2, R, FullR]
  def because(because: (P1, P2) => Boolean): Builder2[P1, P2, R, FullR] = macro Builder2.becauseImpl[P1, P2, R, FullR]
  def becauseHolder(becauseHolder: CodeHolder[(P1, P2) => Boolean]) =
    wrap(currentNodeL.andThen(toScenarioL).andThen(becauseL((so, sn, b) => checkBecause(makeClosures, sn))).set(this, Some(becauseHolder)))
  def matchWith(pf: PartialFunction[(P1, P2), R]) = macro Builder2.matchWithImpl[P1, P2, R, FullR]
  def scenario(p1: P1, p2: P2, title: String = null) =
    wrap(nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) => checkDuplicateScenario(scenarios, new Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]((p1, p2), title = Option(title))) :: nodes))
  def assertionHolder(assertionHolder: CodeHolder[((P1, P2), Either[Exception, R]) => Boolean]) =
    wrap(currentNodeL.andThen(toScenarioL).mod(this, (s) => s.copy(assertions = s.assertions :+ assertionHolder)))
  def configurator(cfg: (P1, P2) => Unit) = wrap(currentNodeL.andThen(toScenarioL).andThen(configuratorL).mod(this, (l) => l :+ ((params: (P1, P2)) => cfg(params._1, params._2))))
  def copyNodes(nodes: List[BuilderNode[R, (P1, P2) => R]]) = wrap(copy(nodes = nodes))
  def build: Engine2[P1, P2, R, FullR] = buildEngine.buildEngine(this, buildExceptions)
  def copyWithNewExceptions(buildExceptions: Map[BuilderNode[R, (P1, P2) => R], List[Exception]]) = wrap(copy(buildExceptions = buildExceptions))
}

class MakeClosures2[P1, P2, R] extends MakeClosures[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] {
  def makeBecauseClosure(s: Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]): BecauseClosure = ((bfn) => wrapBecause(s, { s.executeConfigurators; bfn(s.params._1, s.params._2) }))
  def makeBecauseClosure(params: (P1, P2)): BecauseClosure = ((bfn) => wrapBecause(params, bfn(params._1, params._2)))
  def makeResultClosure(s: Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]): ResultClosure = ((rfn) => { s.executeConfigurators; rfn(s.params._1, s.params._2) })
  def makeResultClosure(params: (P1, P2)): ResultClosure = ((rfn) => rfn(params._1, params._2))
}

class FoldingBuildEngine2[P1, P2, R, FullR] extends SimpleFoldingBuildEngine[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, FullR, Engine2[P1, P2, R, FullR]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode2), new MakeClosures2, BuildEngine.expectedToCode2) {
  def constructEngine(
    requirement: Requirement,
    dts: List[DecisionTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]],
    requirements: BuilderNodeHolder[R, (P1, P2) => R],
    exceptionMap: Map[BuilderNode[R, (P1, P2) => R], List[Exception]],
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR): FoldingEngine2[P1, P2, R, FullR] =
    FoldingEngine2(requirement, dts, evaluateTree, requirements, exceptionMap, initialValue, foldingFn)
}
case class SimpleBuildEngine2[P1, P2, R] extends SimpleBuildEngine[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Engine2[P1, P2, R, R]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode2), new MakeClosures2, BuildEngine.expectedToCode2) {
  def constructEngine(requirement: Requirement, dt: DecisionTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R], requirements: BuilderNodeHolder[R, (P1, P2) => R], exceptionMap: Map[BuilderNode[R, (P1, P2) => R], List[Exception]]) =
    Engine2FromTests(requirement, dt, evaluateTree, requirements, exceptionMap)
}

trait Engine2[P1, P2, R, FullR] extends Engine[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] with Function2[P1, P2, FullR]

case class Engine2FromTests[P1, P2, R](
  asRequirement: Requirement,
  tree: DecisionTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R],
  evaluator: EvaluateTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R],
  requirements: BuilderNodeHolder[R, (P1, P2) => R],
  buildExceptions: Map[BuilderNode[R, (P1, P2) => R], List[Exception]])
  extends Engine2[P1, P2, R, R] with EngineFromTests[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] with Function2[P1, P2, R] {
  def apply(p1: P1, p2: P2) = applyParams((p1, p2))
}
case class FoldingEngine2[P1, P2, R, FullR](
  asRequirement: Requirement,
  trees: List[DecisionTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]],
  evaluator: EvaluateTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R],
  requirements: BuilderNodeHolder[R, (P1, P2) => R],
  buildExceptions: Map[BuilderNode[R, (P1, P2) => R], List[Exception]],
  initialValue: CodeHolder[() => FullR],
  foldingFn: (FullR, R) => FullR)
  extends Engine2[P1, P2, R, FullR] with FoldingEngine[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, FullR] {
  def apply(p1: P1, p2: P2) = applyParams(p1, p2)
}

