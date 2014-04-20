package org.cddcore.engine.builder

import scala.reflect.macros.Context
import org.cddcore.engine._
import org.cddcore.utilities._
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
  buildExceptions: ExceptionMap = new ExceptionMap(),
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
    wrap(nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) => checkDuplicateScenario(bl, this, new Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]((p1, p2), title = Option(title))) :: nodes))
  def assertionHolder(assertionHolder: CodeHolder[((P1, P2), Either[Exception, R]) => Boolean]) =
    wrap(currentNodeL.andThen(toScenarioL).mod(this, (s) => s.copyScenario(assertions = s.assertions :+ assertionHolder)))
  def configurator(cfg: (P1, P2) => Unit) = wrap(currentNodeL.andThen(toScenarioL).andThen(configuratorL).mod(this, (l) => l :+ ((params: (P1, P2)) => cfg(params._1, params._2))))
  def copyNodes(nodes: List[BuilderNode[R, (P1, P2) => R]]) = wrap(copy(nodes = nodes))
  def build: Engine2[P1, P2, R, FullR] = nodes match {
    case (r: BuilderNodeAndHolder[R, (P1, P2) => R]) :: nil => buildEngine.buildEngine(r, buildExceptions)
      case _ => throw new IllegalArgumentException(nodes.toString)
  }
  def copyWithNewExceptions(buildExceptions: ExceptionMap) = wrap(copy(buildExceptions = buildExceptions))
}

class MakeClosures2[P1, P2, R] extends MakeClosures[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] {
  def makeBecauseClosure(s: Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]): BecauseClosure = ((bfn) => wrapBecause(s, { s.executeConfigurators; bfn(s.params._1, s.params._2) }))
  def makeBecauseClosure(params: (P1, P2)): BecauseClosure = ((bfn) => wrapBecause(params, bfn(params._1, params._2)))
  def makeResultClosure(s: Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]): ResultClosure = ((rfn) => { s.executeConfigurators; rfn(s.params._1, s.params._2) })
  def makeResultClosure(params: (P1, P2)): ResultClosure = ((rfn) => rfn(params._1, params._2))
}

class FoldingBuildEngine2[P1, P2, R, FullR] extends SimpleFoldingBuildEngine[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, FullR, Engine2[P1, P2, R, FullR], Engine2[P1, P2, R, R]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode2), new MakeClosures2, BuildEngine.expectedToCode2, BuildEngine.builderEngine2[P1, P2, R]) {
  def constructFoldingEngine(
    requirement: BuilderNodeAndHolder[R, (P1, P2) => R],
    engines: List[EngineFromTests[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]],
    exceptionMap: ExceptionMap,
    initialValue: CodeHolder[() => FullR],
    foldingFn: (FullR, R) => FullR): FoldingEngine2[P1, P2, R, FullR] =
    FoldingEngine2(requirement, engines, evaluateTree, exceptionMap, initialValue, foldingFn)
}
case class SimpleBuildEngine2[P1, P2, R] extends SimpleBuildEngine[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, Engine2[P1, P2, R, R]](
  BuildEngine.defaultRoot(BuildEngine.defaultRootCode2), new MakeClosures2, BuildEngine.expectedToCode2) {
  def constructEngine(requirement: BuilderNodeAndHolder[R, (P1, P2) => R], dt: DecisionTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R],
    exceptionMap: ExceptionMap) =
    Engine2FromTests(requirement, dt, evaluateTree, exceptionMap)
}

trait Engine2[P1, P2, R, FullR] extends Engine[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] with Function2[P1, P2, FullR]

case class Engine2FromTests[P1, P2, R](
  asRequirement: BuilderNodeAndHolder[R, (P1, P2) => R],
  tree: DecisionTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R],
  evaluator: EvaluateTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R],
  buildExceptions: ExceptionMap)
  extends Engine2[P1, P2, R, R] with EngineFromTests[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] with Function2[P1, P2, R] {
  def apply(p1: P1, p2: P2) = applyParams((p1, p2))
}

case class FoldingEngine2[P1, P2, R, FullR](
  asRequirement: BuilderNodeAndHolder[R, (P1, P2) => R],
  engines: List[EngineFromTests[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]],
  evaluator: EvaluateTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R],
  buildExceptions: ExceptionMap,
  initialValue: CodeHolder[() => FullR],
  foldingFn: (FullR, R) => FullR)
  extends Engine2[P1, P2, R, FullR] with FoldingEngine[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, FullR] {
  def apply(p1: P1, p2: P2) = applyParams(p1, p2)
}

