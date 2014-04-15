package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros

class BuilderLens3[P1, P2, P3, R, B <: BuilderNodeHolder[R, (P1, P2, P3) => R]] extends FullBuilderLens[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, B]
object Builder3 {
  def bl[P1, P2, P3, R]() = new BuilderLens3[P1, P2, P3, R, Builder3[P1, P2, P3, R]]
  def expectedToCode[P1, P2, P3, R]: Either[Exception, R] => CodeHolder[(P1, P2, P3) => R] =
    (x) => new CodeHolder((p1, p2, p3) => x match { case Right(r) => r }, x.toString())
  def creator[P1, P2, P3, R](requirements: BuilderNodeHolder[R, (P1, P2, P3) => R]) =
    (r: DecisionTreeNode[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R],
      buildExceptions: Map[BuilderNode[R, (P1, P2, P3) => R], List[Exception]]) => new Engine3(r, requirements, buildExceptions)
  def becauseImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(because: c.Expr[(P1, P2, P3) => Boolean]): c.Expr[Builder3[P1, P2, P3, R]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, P3, R]()
      val ch = CodeHolder[(P1, P2, P3) => Boolean](because.splice, c.literal(show(because.tree)).splice)
      val thisObject: Builder3[P1, P2, P3, R] = (c.Expr[Builder3[P1, P2, P3, R]](c.prefix.tree)).splice
      thisObject.becauseHolder(ch)
    }
  }
  def codeImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(code: c.Expr[(P1, P2, P3) => R]): c.Expr[Builder3[P1, P2, P3, R]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, P3, R]()
      val ch = CodeHolder[(P1, P2, P3) => R](code.splice, c.literal(show(code.tree)).splice)
      val thisObject: Builder3[P1, P2, P3, R] = (c.Expr[Builder3[P1, P2, P3, R]](c.prefix.tree)).splice
      thisObject.codeHolder(ch)
    }
  }
  def matchWithImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(pf: c.Expr[PartialFunction[(P1, P2, P3), R]]): c.Expr[Builder3[P1, P2, P3, R]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, P3, R]()
      val chBecause = CodeHolder[(P1, P2, P3) => Boolean]((p1, p2, p3) => pf.splice.isDefinedAt((p1, p2, p3)), c.literal(show(pf.tree)).splice)
      val chResult = CodeHolder[(P1, P2, P3) => R]((p1, p2, p3) => pf.splice.apply((p1, p2, p3)), c.literal(show(pf.tree)).splice)
      val thisObject: Builder3[P1, P2, P3, R] = (c.Expr[Builder3[P1, P2, P3, R]](c.prefix.tree)).splice

      thisObject.becauseHolder(chBecause).codeHolder(chResult)
    }
  }
}

trait Builder3[P1, P2, P3, R] extends Builder[R, (P1, P2, P3) => R, Builder3[P1, P2, P3, R]]
  with BuilderWithModifyChildrenForBuild[R, (P1, P2, P3) => R]
  with ValidateScenario[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]
  with MakeClosures3[P1, P2, P3, R] {

  val bl3 = new BuilderLens3[P1, P2, P3, R, Builder3[P1, P2, P3, R]]
  import bl3._
  lazy val scenarios = all(classOf[Scenario[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]]).toSet

  def scenario(p1: P1, p2: P2, p3: P3, title: String = null) = wrap(nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) => checkDuplicateScenario(new Scenario[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]((p1, p2, p3), Option(title))) :: nodes))
  def because(because: (P1, P2, P3) => Boolean) = macro Builder3.becauseImpl[P1, P2, P3, R]
  def becauseHolder(becauseHolder: CodeHolder[(P1, P2, P3) => Boolean]) =
    wrap(currentNodeL.andThen(toScenarioL).andThen(becauseL((so, sn, b) => checkBecause(sn))).set(this, Some(becauseHolder)))
  def assertionHolder(assertionHolder: CodeHolder[((P1, P2, P3), Either[Exception, R]) => Boolean]) =
    wrap(currentNodeL.andThen(toScenarioL).mod(this, (s) => s.copy(assertions = s.assertions :+ assertionHolder)))
  def code(code: (P1, P2, P3) => R) = macro Builder3.codeImpl[P1, P2, P3, R]
  def matchWith(pf: PartialFunction[(P1, P2, P3), R]) = macro Builder3.matchWithImpl[P1, P2, P3, R]
  def configurator(cfg: (P1, P2, P3) => Unit) = wrap(currentNodeL.andThen(toScenarioL).andThen(configuratorL).mod(this, _ :+ ((params: (P1, P2, P3)) => cfg(params._1, params._2, params._3))))

  def copyNodes(nodes: List[BuilderNode[R, (P1, P2, P3) => R]]) = new Builder3Class[P1, P2, P3, R](nodes, buildExceptions)
  def build: Engine3[P1, P2, P3, R] = BuildEngine.build3(this)
  def copyWithNewExceptions(buildExceptions: Map[BuilderNode[R, (P1, P2, P3) => R], List[Exception]]) = new Builder3Class[P1, P2, P3, R](nodes, buildExceptions)

}

case class Builder3Class[P1, P2, P3, R](
  nodes: List[BuilderNode[R, (P1, P2, P3) => R]] = List(new EngineDescription[R, (P1, P2, P3) => R]),
  buildExceptions: Map[BuilderNode[R, (P1, P2, P3) => R], List[Exception]] = Map[BuilderNode[R, (P1, P2, P3) => R], List[Exception]]())(implicit val ldp: LoggerDisplayProcessor)
  extends Builder3[P1, P2, P3, R]

trait MakeClosures3[P1, P2, P3, R] extends MakeClosures[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R] {
  def makeBecauseClosure(s: Scenario[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]): BecauseClosure =
    ((bfn) => { s.executeConfigurators; bfn(s.params._1, s.params._2, s.params._3) })
  def makeBecauseClosure(params: (P1, P2, P3)): BecauseClosure =
    ((bfn) => bfn(params._1, params._2, params._3))

  def makeResultClosure(s: Scenario[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]): ResultClosure =
    ((rfn) => { s.executeConfigurators; rfn(s.params._1, s.params._2, s.params._3) })
  def makeResultClosure(params: (P1, P2, P3)): ResultClosure =
    ((rfn) => rfn(params._1, params._2, params._3))

}

trait EvaluateTree3[P1, P2, P3, R] extends EvaluateTree[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R] with MakeClosures3[P1, P2, P3, R] {
}
class DecisionTreeLens3[P1, P2, P3, R] extends DecisionTreeLens[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R] {
  def creator(requirements: BuilderNodeHolder[R, (P1, P2, P3) => R]) = Builder3.creator[P1, P2, P3, R](requirements)
}

case class Engine3[P1, P2, P3, R](
  root: DecisionTreeNode[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R],
  requirements: BuilderNodeHolder[R, (P1, P2, P3) => R],
  buildExceptions: Map[BuilderNode[R, (P1, P2, P3) => R], List[Exception]],
  rootIsDefault: Boolean = false)
  extends EngineAndDecisionTree[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R]
  with EvaluateTree3[P1, P2, P3, R]
  with Function3[P1, P2, P3, R] {
  def apply(p1: P1, p2: P2, p3: P3) = evaluate(root, (p1, p2, p3))
  val lens = new DecisionTreeLens3[P1, P2, P3, R]
  val expectedToCode: Either[Exception, R] => CodeHolder[(P1, P2, P3) => R] = Builder3.expectedToCode[P1, P2, P3, R]
}
