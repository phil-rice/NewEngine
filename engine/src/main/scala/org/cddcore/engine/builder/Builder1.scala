package org.cddcore.engine.builder

import scala.reflect.macros.Context
import org.cddcore.engine._
import org.cddcore.utilities._
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
  def matchOnImpl[P: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(pf: c.Expr[PartialFunction[(P), R]]): c.Expr[Builder1[P, R, FullR]] = {
    import c.universe._
    reify {
      val thisObject: Builder1[P, R, FullR] = (c.Expr[Builder1[P, R, FullR]](c.prefix.tree)).splice
      val literal = c.literal(show(pf.tree)).splice
      thisObject.matchOnPrim(pf.splice, literal, literal)
    }
  }
}
case class Builder1[P, R, FullR](
  nodes: List[BuilderNode[R, (P) => R]] = List(new EngineDescription[P, (P) => Boolean, R, (P) => R]),
  buildExceptions: ExceptionMap = new ExceptionMap(),
  buildEngine: BuildEngine[P, (P) => Boolean, R, (P) => R, FullR, Engine1[P, R, FullR]])(implicit val ldp: LoggerDisplayProcessor)

  extends Builder[P, (P) => Boolean, R, (P) => R, FullR, Builder1[P, R, FullR], Engine1[P, R, FullR]] {
  val makeClosures = buildEngine.mc
  val validator = buildEngine.validator
  import bl._
  import makeClosures._
  import validator._

  def because(because: (P) => Boolean): Builder1[P, R, FullR] = macro Builder1.becauseImpl[P, R, FullR]
  def code(code: (P) => R): Builder1[P, R, FullR] = macro Builder1.codeImpl[P, R, FullR]
  def matchOn(pf: PartialFunction[P, R]) = macro Builder1.matchOnImpl[P, R, FullR]
  def matchOnPrim(pf: PartialFunction[(P), R], becauseToString: String, resultToString: String) = {
    val chBecause = CodeHolder[(P) => Boolean]((p) => pf.isDefinedAt(p), becauseToString)
    val chResult = CodeHolder[(P) => R](p => pf.apply(p), resultToString)
    becauseHolder(chBecause).codeHolder(chResult)
  }
  def scenario(p: P, title: String = null) = wrap(nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) =>
    checkDuplicateScenario(bl, this, new Scenario[P, (P) => Boolean, R, (P) => R](p, title = Option(title))) :: nodes))
  def assertionHolder(assertionHolder: CodeHolder[(P, Either[Exception, R]) => Boolean]) =
    wrap(currentNodeL.andThen(toScenarioL).mod(this, (s) => s.copyScenario(assertions = s.assertions :+ assertionHolder)))
  def configurator(cfg: (P) => Unit) = wrap(currentNodeL.andThen(toScenarioL).andThen(configuratorL).mod(this, _ :+ cfg))
  def copyNodes(nodes: List[BuilderNode[R, (P) => R]]) = wrap(copy(nodes = nodes))
  def build: Engine1[P, R, FullR] = nodes match {
    case (r: EngineAsRequirement[P, (P) => Boolean, R, (P) => R]) :: nil => buildEngine.buildEngine(r, buildExceptions)
    case _ => throw new IllegalArgumentException(nodes.toString)
  }
  def copyWithNewExceptions(buildExceptions: ExceptionMap) =
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
    requirement: EngineAsRequirement[P, (P) => Boolean, R, (P) => R],
    dt: DecisionTree[P, (P) => Boolean, R, (P) => R],
    exceptionMap: ExceptionMap): Engine1[P, R, R] =
    Engine1FromTests(requirement, dt, evaluateTree, exceptionMap)

}
class FoldingBuildEngine1[P, R, FullR] extends SimpleFoldingBuildEngine[P, (P) => Boolean, R, (P) => R, FullR, Engine1[P, R, FullR], Engine1[P, R, R]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode1), new MakeClosures1, BuildEngine.expectedToCode1, BuildEngine.builderEngine1[P, R]) {

  def constructFoldingEngine(
    requirement: EngineAsRequirement[P, (P) => Boolean, R, (P) => R],
    engines: List[EngineFromTests[P, (P) => Boolean, R, (P) => R]],
    exceptionMap: ExceptionMap,
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR): FoldingEngine1[P, R, FullR] =
    FoldingEngine1(requirement, engines, evaluateTree, exceptionMap, initialValue, foldingFn)
}
trait Engine1[P, R, FullR] extends EngineTools[P, (P) => Boolean, R, (P) => R] with Function1[P, FullR]

case class Engine1FromTests[P, R](
  asRequirement: EngineAsRequirement[P, (P) => Boolean, R, (P) => R],
  tree: DecisionTree[P, (P) => Boolean, R, (P) => R],
  evaluator: EvaluateTree[P, (P) => Boolean, R, (P) => R],
  buildExceptions: ExceptionMap,
  val textOrder: Int = Reportable.nextTextOrder)
  extends Engine1[P, R, R] with EngineFromTests[P, (P) => Boolean, R, (P) => R] with Function1[P, R] {
  def apply(p: P) = applyParams(p)

}
case class FoldingEngine1[P, R, FullR](
  asRequirement: EngineAsRequirement[P, (P) => Boolean, R, (P) => R],
  engines: List[EngineFromTests[P, (P) => Boolean, R, (P) => R]],
  evaluator: EvaluateTree[P, (P) => Boolean, R, (P) => R],
  buildExceptions: ExceptionMap,
  initialValue: CodeHolder[() => FullR],
  foldingFn: (FullR, R) => FullR,
  val textOrder: Int = Reportable.nextTextOrder)
  extends Engine1[P, R, FullR] with FoldingEngine[P, (P) => Boolean, R, (P) => R, FullR] {
  def apply(p: P) = applyParams(p)
}
trait DecisionTreeBuilderForTests1[P, R] extends DecisionTreeBuilderForTests[P, (P) => Boolean, R, (P) => R]{
    def expectedToCode = BuildEngine.expectedToCode1[P, R]
}
