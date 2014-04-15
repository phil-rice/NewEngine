package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros

class BuilderLens2[P1, P2, R, B <: EngineNodeHolder[R, (P1, P2) => R]] extends FullBuilderLens[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, B]

object Builder2 {
  def bl[P1, P2, R]() = new BuilderLens2[P1, P2, R, Builder2[P1, P2, R]]
  def expectedToCode[P1, P2, R]: Either[Exception, R] => CodeHolder[(P1, P2) => R] =
    (x) => new CodeHolder((p1, p2) => x match { case Right(r) => r }, x.toString())
  def creator[P1, P2, R](requirements: EngineNodeHolder[R, (P1, P2) => R]) =
    (r: DecisionTreeNode[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R],
      buildExceptions: Map[EngineNode[R, (P1, P2) => R], List[Exception]]) => new Engine2(r, requirements, buildExceptions)
  def becauseImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(because: c.Expr[(P1, P2) => Boolean]): c.Expr[Builder2[P1, P2, R]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, R]()
      val ch = CodeHolder[(P1, P2) => Boolean](because.splice, c.literal(show(because.tree)).splice)
      val thisObject: Builder2[P1, P2, R] = (c.Expr[Builder2[P1, P2, R]](c.prefix.tree)).splice
      thisObject.becauseHolder(ch)
    }
  }
  def codeImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(code: c.Expr[(P1, P2) => R]): c.Expr[Builder2[P1, P2, R]] = {
    import c.universe._
    reify {
      val l = bl[P1, P2, R]()
      val ch = CodeHolder[(P1, P2) => R](code.splice, c.literal(show(code.tree)).splice)
      val thisObject: Builder2[P1, P2, R] = (c.Expr[Builder2[P1, P2, R]](c.prefix.tree)).splice
      thisObject.codeHolder(ch)
    }
  }
  def matchWithImpl[P1: c.WeakTypeTag, P2: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(pf: c.Expr[PartialFunction[(P1, P2), R]]): c.Expr[Builder2[P1, P2, R]] = {
    import c.universe._
    val thisObject: Builder2[P1, P2, R] = (c.Expr[Builder2[P1, P2, R]](c.prefix.tree)).splice
    reify {
      val l = bl[P1, P2, R]()
      val chBecause = CodeHolder[(P1, P2) => Boolean]((p1, p2) => pf.splice.isDefinedAt((p1, p2)), c.literal(show(pf.tree)).splice)
      val chResult = CodeHolder[(P1, P2) => R]((p1, p2) => pf.splice.apply((p1, p2)), c.literal(show(pf.tree)).splice)

      thisObject.becauseHolder(chBecause).codeHolder(chResult)
    }
  }
}
case class Builder2[P1, P2, R](
  nodes: List[EngineNode[R, (P1, P2) => R]] = List(new EngineDescription[R, (P1, P2) => R]),
  buildExceptions: Map[EngineNode[R, (P1, P2) => R], List[Exception]] = Map[EngineNode[R, (P1, P2) => R], List[Exception]]())(implicit val ldp: LoggerDisplayProcessor)
  extends Builder[R, (P1, P2) => R, Builder2[P1, P2, R]]
  with BuilderWithModifyChildrenForBuild[R, (P1, P2) => R]
  with ValidateScenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]
  with MakeClosures2[P1, P2, R] {
  val bl2 = new BuilderLens2[P1, P2, R, Builder2[P1, P2, R]]
  import bl2._
  lazy val scenarios = all(classOf[Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]]).toSet

  def code(code: (P1, P2) => R): Builder2[P1, P2, R] = macro Builder2.codeImpl[P1, P2, R]
  def because(because: (P1, P2) => Boolean): Builder2[P1, P2, R] = macro Builder2.becauseImpl[P1, P2, R]
  def becauseHolder(becauseHolder: CodeHolder[(P1, P2) => Boolean]) =
    currentNodeL.andThen(toScenarioL).andThen(becauseL((so, sn, b) => checkBecause(sn))).set(this, Some(becauseHolder))
  def matchWith(pf: PartialFunction[(P1, P2), R]) = macro Builder2.matchWithImpl[P1, P2, R]
  def scenario(p1: P1, p2: P2, title: String = null) = nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) => checkDuplicateScenario(new Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]((p1, p2), title = Option(title))) :: nodes)
  def assertionHolder(assertionHolder: CodeHolder[((P1, P2), Either[Exception, R]) => Boolean]) =
    currentNodeL.andThen(toScenarioL).mod(this, (s) => s.copy(assertions = s.assertions :+ assertionHolder))
  def configurator(cfg: (P1, P2) => Unit) = currentNodeL.andThen(toScenarioL).andThen(configuratorL).mod(this, (l) => l :+ ((params: (P1, P2)) => cfg(params._1, params._2)))
  def copyNodes(nodes: List[EngineNode[R, (P1, P2) => R]]) = new Builder2[P1, P2, R](nodes)
  def build: Engine2[P1, P2, R] = BuildEngine.build2(this)

}

trait MakeClosures2[P1, P2, R] extends MakeClosures[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] {
  def makeBecauseClosure(s: Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]): BecauseClosure = ((bfn) => { s.executeConfigurators; bfn(s.params._1, s.params._2) })
  def makeBecauseClosure(params: (P1, P2)): BecauseClosure = ((bfn) => bfn(params._1, params._2))
  def makeResultClosure(s: Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R]): ResultClosure = ((rfn) => { s.executeConfigurators; rfn(s.params._1, s.params._2) })
  def makeResultClosure(params: (P1, P2)): ResultClosure = ((rfn) => rfn(params._1, params._2))

}
trait EvaluateTree2[P1, P2, R] extends EvaluateTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] with MakeClosures2[P1, P2, R] {
}
class DecisionTreeLens2[P1, P2, R] extends DecisionTreeLens[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] {
  def creator(requirements: EngineNodeHolder[R, (P1, P2) => R]) = Builder2.creator(requirements)
}

case class Engine2[P1, P2, R](root: DecisionTreeNode[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R],
  requirements: EngineNodeHolder[R, (P1, P2) => R],
  buildExceptions: Map[EngineNode[R, (P1, P2) => R], List[Exception]] = Map[EngineNode[R, (P1, P2) => R], List[Exception]](),
  rootIsDefault: Boolean = false) extends EngineAndDecisionTree[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R] with EvaluateTree2[P1, P2, R] with Function2[P1, P2, R] {
  val lens = new DecisionTreeLens2[P1, P2, R]
  def apply(p1: P1, p2: P2) = evaluate(root, (p1, p2))
  val expectedToCode: Either[Exception, R] => CodeHolder[(P1, P2) => R] = Builder2.expectedToCode
}
