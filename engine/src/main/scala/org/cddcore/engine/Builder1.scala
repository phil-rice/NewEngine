package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros

object Builder1 {
  def bl[P, R, FullR]() = new FullBuilderLens[P, (P) => Boolean, R, (P) => R, FullR, Builder1[P, R, FullR]]

  def becauseImpl[P: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(because: c.Expr[(P) => Boolean]): c.Expr[Builder1[P, R, FullR]] = {
    import c.universe._
    reify {
      val l = bl[P, R, FullR]()
      val ch = CodeHolder[(P) => Boolean](because.splice, c.literal(show(because.tree)).splice)
      val thisObject: Builder1[P, R, FullR] = (c.Expr[Builder1[P, R, FullR]](c.prefix.tree)).splice
      thisObject.becauseHolder(ch)
    }
  }
  def codeImpl[P: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(code: c.Expr[(P) => R]): c.Expr[Builder1[P, R, FullR]] = {
    import c.universe._
    reify {
      val l = bl[P, R, FullR]()
      val ch = CodeHolder[(P) => R](code.splice, c.literal(show(code.tree)).splice)
      val thisObject: Builder1[P, R, FullR] = (c.Expr[Builder1[P, R, FullR]](c.prefix.tree)).splice
      thisObject.codeHolder(ch)
    }
  }
}
case class Builder1[P, R, FullR](
  nodes: List[BuilderNode[R, (P) => R]] = List(new EngineDescription[R, (P) => R]),
  buildExceptions: Map[BuilderNode[R, (P) => R], List[Exception]] = Map[BuilderNode[R, (P) => R], List[Exception]](),
  buildEngine: BuildEngine[P, (P) => Boolean, R, (P) => R, FullR, Engine1[P, R, FullR]])(implicit val ldp: LoggerDisplayProcessor)

  extends Builder[P, (P) => Boolean, R, (P) => R, FullR, Builder1[P, R, FullR], Engine1[P, R, FullR]] {
  val makeClosures = buildEngine.mc
  val validator = buildEngine.validator
  import bl._
  import makeClosures._
  import validator._

  def because(because: (P) => Boolean): Builder1[P, R, FullR] = macro Builder1.becauseImpl[P, R, FullR]
  def code(code: (P) => R): Builder1[P, R, FullR] = macro Builder1.codeImpl[P, R, FullR]

  def becauseHolder(becauseHolder: CodeHolder[P => Boolean]) = wrap(currentNodeL.andThen(toScenarioL).andThen(becauseL((so, sn, b) => checkBecause(makeClosures, sn))).set(this, Some(becauseHolder)))
  def scenario(p: P, title: String = null) = wrap(nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) =>
    checkDuplicateScenario(scenarios, new Scenario[P, (P) => Boolean, R, (P) => R](p, title = Option(title))) :: nodes))
  def assertionHolder(assertionHolder: CodeHolder[(P, Either[Exception, R]) => Boolean]) =
    wrap(currentNodeL.andThen(toScenarioL).mod(this, (s) => s.copy(assertions = s.assertions :+ assertionHolder)))
  def configurator(cfg: (P) => Unit) = wrap(currentNodeL.andThen(toScenarioL).andThen(configuratorL).mod(this, _ :+ cfg))
  def copyNodes(nodes: List[BuilderNode[R, (P) => R]]) = wrap(copy(nodes = nodes))
  def build: Engine1[P, R, FullR] = buildEngine.buildEngine(this, buildExceptions)
  def copyWithNewExceptions(buildExceptions: Map[BuilderNode[R, (P) => R], List[Exception]]) =
    wrap(copy(buildExceptions = buildExceptions))
}

class MakeClosures1[P, R] extends MakeClosures[P, (P) => Boolean, R, (P) => R] {
  def makeBecauseClosure(s: Scenario[P, (P) => Boolean, R, (P) => R]): BecauseClosure = (bfn) => wrapBecause(s, { s.executeConfigurators; bfn(s.params) })
  def makeBecauseClosure(params: P): BecauseClosure = (bfn) => wrapBecause(params, bfn(params))
  def makeResultClosure(s: Scenario[P, (P) => Boolean, R, (P) => R]): ResultClosure = (rfn) => { s.executeConfigurators; rfn(s.params) }
  def makeResultClosure(params: P): ResultClosure = (rfn) => rfn(params)
}
case class SimpleBuildEngine1[P, R] extends SimpleBuildEngine[P, (P) => Boolean, R, (P) => R, Engine1[P, R, R]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode1), new MakeClosures1, BuildEngine.expectedToCode1) {
  def constructEngine(
    requirement: Requirement,
    dt: DecisionTree[P, (P) => Boolean, R, (P) => R],
    requirements: BuilderNodeHolder[R, (P) => R],
    exceptionMap: Map[BuilderNode[R, (P) => R], List[Exception]]): Engine1[P, R, R] =
    Engine1FromTests(requirement, dt, evaluateTree, requirements, exceptionMap)

}
class FoldingBuildEngine1[P, R, FullR] extends SimpleFoldingBuildEngine[P, (P) => Boolean, R, (P) => R, FullR, Engine1[P, R, FullR]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode1), new MakeClosures1, BuildEngine.expectedToCode1) {
  def constructEngine(
    requirement: Requirement,
    dts: List[DecisionTree[P, (P) => Boolean, R, (P) => R]],
    requirements: BuilderNodeHolder[R, (P) => R],
    exceptionMap: Map[BuilderNode[R, (P) => R], List[Exception]],
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR): FoldingEngine1[P, R, FullR] =
    FoldingEngine1(requirement, dts, evaluateTree, requirements, exceptionMap, initialValue, foldingFn)
}
trait Engine1[P, R, FullR] extends Engine[P, (P) => Boolean, R, (P) => R] with Function1[P, FullR]

case class Engine1FromTests[P, R](
  asRequirement: Requirement,
  tree: DecisionTree[P, (P) => Boolean, R, (P) => R],
  evaluator: EvaluateTree[P, (P) => Boolean, R, (P) => R],
  requirements: BuilderNodeHolder[R, (P) => R],
  buildExceptions: Map[BuilderNode[R, (P) => R], List[Exception]])
  extends Engine1[P, R, R] with EngineFromTests[P, (P) => Boolean, R, (P) => R] with Function1[P, R] {
  def apply(p: P) = applyParams(p)

}
case class FoldingEngine1[P, R, FullR](
  asRequirement: Requirement,
  trees: List[DecisionTree[P, (P) => Boolean, R, (P) => R]],
  evaluator: EvaluateTree[P, (P) => Boolean, R, (P) => R],
  requirements: BuilderNodeHolder[R, (P) => R],
  buildExceptions: Map[BuilderNode[R, (P) => R], List[Exception]],
  initialValue: CodeHolder[() => FullR],
  foldingFn: (FullR, R) => FullR)
  extends Engine1[P, R, FullR] with FoldingEngine[P, (P) => Boolean, R, (P) => R, FullR] {
  def apply(p: P) = applyParams(p)
}

