package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros

object Builder3 {
  def bl[P1, P2, P3, R, FullR]() = new FullBuilderLens[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, FullR, Builder3[P1, P2, P3, R, FullR]]
  def expectedToCode[P1, P2, P3, R]: Either[Exception, R] => CodeHolder[(P1, P2, P3) => R] = (x) => new CodeHolder((p1, p2, p3) => x match { case Right(r) => r }, x.toString())

  def becauseImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(because: c.Expr[(P1, P2, P3) => Boolean]): c.Expr[Builder3[P1, P2, P3, R, FullR]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, P3, R, FullR]()
      val ch = CodeHolder[(P1, P2, P3) => Boolean](because.splice, c.literal(show(because.tree)).splice)
      val thisObject: Builder3[P1, P2, P3, R, FullR] = (c.Expr[Builder3[P1, P2, P3, R, FullR]](c.prefix.tree)).splice
      thisObject.becauseHolder(ch)
    }
  }
  def codeImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(code: c.Expr[(P1, P2, P3) => R]): c.Expr[Builder3[P1, P2, P3, R, FullR]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, P3, R, FullR]()
      val ch = CodeHolder[(P1, P2, P3) => R](code.splice, c.literal(show(code.tree)).splice)
      val thisObject: Builder3[P1, P2, P3, R, FullR] = (c.Expr[Builder3[P1, P2, P3, R, FullR]](c.prefix.tree)).splice
      thisObject.codeHolder(ch)
    }
  }
  def matchWithImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag, FullR: c.WeakTypeTag](c: Context)(pf: c.Expr[PartialFunction[(P1, P2, P3), R]]): c.Expr[Builder3[P1, P2, P3, R, FullR]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, P3, R, FullR]()
      val chBecause = CodeHolder[(P1, P2, P3) => Boolean]((p1, p2, p3) => pf.splice.isDefinedAt((p1, p2, p3)), c.literal(show(pf.tree)).splice)
      val chResult = CodeHolder[(P1, P2, P3) => R]((p1, p2, p3) => pf.splice.apply((p1, p2, p3)), c.literal(show(pf.tree)).splice)
      val thisObject: Builder3[P1, P2, P3, R, FullR] = (c.Expr[Builder3[P1, P2, P3, R, FullR]](c.prefix.tree)).splice

      thisObject.becauseHolder(chBecause).codeHolder(chResult)
    }
  }
}
case class Builder3[P1, P2, P3, R, FullR](
  nodes: List[BuilderNode[R, (P1, P2, P3) => R]] = List(new EngineDescription[R, (P1, P2, P3) => R]),
  buildExceptions: Map[BuilderNode[R, (P1, P2, P3) => R], List[Exception]] = Map[BuilderNode[R, (P1, P2, P3) => R], List[Exception]](),
  buildEngine: BuildEngine[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, FullR, Engine3[P1, P2, P3, R, FullR]])(implicit val ldp: LoggerDisplayProcessor)
  extends Builder[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, FullR, Builder3[P1, P2, P3, R, FullR], Engine3[P1, P2, P3, R, FullR]] {

  val makeClosures = buildEngine.mc
  val validator = buildEngine.validator
  import bl._
  import makeClosures._
  import validator._

  def scenario(p1: P1, p2: P2, p3: P3, title: String = null) = wrap(nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) => checkDuplicateScenario(scenarios, new Scenario[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]((p1, p2, p3), Option(title))) :: nodes))
  def because(because: (P1, P2, P3) => Boolean) = macro Builder3.becauseImpl[P1, P2, P3, R, FullR]
  def becauseHolder(becauseHolder: CodeHolder[(P1, P2, P3) => Boolean]) =
    wrap(currentNodeL.andThen(toScenarioL).andThen(becauseL((so, sn, b) => checkBecause(makeClosures, sn))).set(this, Some(becauseHolder)))
  def assertionHolder(assertionHolder: CodeHolder[((P1, P2, P3), Either[Exception, R]) => Boolean]) =
    wrap(currentNodeL.andThen(toScenarioL).mod(this, (s) => s.copy(assertions = s.assertions :+ assertionHolder)))
  def code(code: (P1, P2, P3) => R) = macro Builder3.codeImpl[P1, P2, P3, R, FullR]
  def matchWith(pf: PartialFunction[(P1, P2, P3), R]) = macro Builder3.matchWithImpl[P1, P2, P3, R, FullR]
  def configurator(cfg: (P1, P2, P3) => Unit) = wrap(currentNodeL.andThen(toScenarioL).andThen(configuratorL).mod(this, _ :+ ((params: (P1, P2, P3)) => cfg(params._1, params._2, params._3))))

  def copyNodes(nodes: List[BuilderNode[R, (P1, P2, P3) => R]]) = wrap(copy(nodes = nodes))
  def build: Engine3[P1, P2, P3, R, FullR] = buildEngine.buildEngine(this, buildExceptions)
  def copyWithNewExceptions(buildExceptions: Map[BuilderNode[R, (P1, P2, P3) => R], List[Exception]]) = wrap(copy(buildExceptions = buildExceptions))
}

class MakeClosures3[P1, P2, P3, R] extends MakeClosures[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R] {
  def makeBecauseClosure(s: Scenario[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]): BecauseClosure =
    ((bfn) => { s.executeConfigurators; bfn(s.params._1, s.params._2, s.params._3) })
  def makeBecauseClosure(params: (P1, P2, P3)): BecauseClosure =
    ((bfn) => bfn(params._1, params._2, params._3))

  def makeResultClosure(s: Scenario[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]): ResultClosure =
    ((rfn) => { s.executeConfigurators; rfn(s.params._1, s.params._2, s.params._3) })
  def makeResultClosure(params: (P1, P2, P3)): ResultClosure =
    ((rfn) => rfn(params._1, params._2, params._3))
}

trait Engine3[P1, P2, P3, R, FullR] extends Engine[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R] with Function3[P1, P2, P3, FullR]

case class Engine3FromTests[P1, P2, P3, R](
  tree: DecisionTree[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R],
  evaluator: EvaluateTree[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R],
  requirements: BuilderNodeHolder[R, (P1, P2, P3) => R],
  buildExceptions: Map[BuilderNode[R, (P1, P2, P3) => R], List[Exception]])
  extends Engine3[P1, P2, P3, R, R] with EngineFromTests[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R] {
  def apply(p1: P1, p2: P2, p3: P3) = evaluator.evaluate(tree, (p1, p2, p3))
  def applyParams(params: (P1, P2, P3)) = evaluator.evaluate(tree, params)
}
case class FoldingEngine3[P1, P2, P3, R, FullR](
  trees: List[DecisionTree[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]],
  evaluator: EvaluateTree[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R],
  requirements: BuilderNodeHolder[R, (P1, P2, P3) => R],
  buildExceptions: Map[BuilderNode[R, (P1, P2, P3) => R], List[Exception]],
  initialValue: CodeHolder[() => FullR],
  foldingFn: (FullR, R) => FullR)
  extends Engine3[P1, P2, P3, R, FullR]
  with FoldingEngine[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, FullR] with Function3[P1, P2, P3, FullR] {
  def apply(p1: P1, p2: P2, p3: P3) = applyParams(p1, p2, p3)
}
