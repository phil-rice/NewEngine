package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros

class BuilderLens1[P, R, B <: EngineNodeHolder[R, (P) => R]] extends FullBuilderLens[P, (P) => Boolean, R, (P) => R, B]

object Builder1 {
  def bl[P, R]() = new BuilderLens1[P, R, Builder1[P, R]]
  def expectedToCode[P, R]: Either[Exception, R] => CodeHolder[(P) => R] =
    (x) => new CodeHolder((p) => x match { case Right(r) => r }, x.toString())

  def creator[P, R](requirements: EngineNodeHolder[R, (P) => R]) = (r: DecisionTreeNode[P, (P) => Boolean, R, (P) => R]) => new Engine1(r, requirements)
  def becauseImpl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(because: c.Expr[(P) => Boolean]): c.Expr[Builder1[P, R]] = {
    import c.universe._
    reify {
      val l = bl[P, R]()
      val ch = CodeHolder[(P) => Boolean](because.splice, c.literal(show(because.tree)).splice)
      val thisObject: Builder1[P, R] = (c.Expr[Builder1[P, R]](c.prefix.tree)).splice
      thisObject.becauseHolder(ch)
    }
  }
  def codeImpl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(code: c.Expr[(P) => R]): c.Expr[Builder1[P, R]] = {
    import c.universe._
    reify {
      val l = bl[P, R]()
      val ch = CodeHolder[(P) => R](code.splice, c.literal(show(code.tree)).splice)
      val thisObject: Builder1[P, R] = (c.Expr[Builder1[P, R]](c.prefix.tree)).splice
      thisObject.codeHolder(ch)
    }
  }
  //  def matchWithImpl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(pf: c.Expr[PartialFunction[P, R]]): c.Expr[Builder1[P, R]] = {
  //    import c.universe._
  //    val thisObject: c.Expr[Builder1[P, R]] = (c.Expr[Builder1[P, R]](c.prefix.tree))
  //    reify {
  //      val ch = CodeHolder(pf.splice, c.literal(show(pf.tree)).splice)
  //      thisObject.splice.matchWithPrim(ch)
  //    }
  //  }
}

case class Builder1[P, R](nodes: List[EngineNode[R, (P) => R]] = List(new EngineDescription[R, (P) => R]))(implicit val ldp: LoggerDisplayProcessor)
  extends Builder[R, (P) => R, Builder1[P, R]]
  with BuilderWithModifyChildrenForBuild[R, (P) => R]
  with ValidateScenario[P, (P) => Boolean, R, (P) => R]
  with MakeClosures1[P, R] {
  val bl1 = Builder1.bl[P, R]()
  import bl1._
  lazy val scenarios = all(classOf[Scenario[P, (P) => Boolean, R, (P) => R]]).toSet
  def because(because: (P) => Boolean): Builder1[P, R] = macro Builder1.becauseImpl[P, R]
  def code(code: (P) => R): Builder1[P, R] = macro Builder1.codeImpl[P, R]

  def becauseHolder(becauseHolder: CodeHolder[P => Boolean]) =
    currentNodeL.andThen(toScenarioL).andThen(becauseL((so, sn, b) => checkBecause(sn))).set(this, Some(becauseHolder))
  def scenario(p: P, title: String = null) = nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) =>
    checkDuplicateScenario(new Scenario[P, (P) => Boolean, R, (P) => R](p, title = Option(title))) :: nodes)
  def assertionHolder(assertionHolder: CodeHolder[(P, Either[Exception, R]) => Boolean]) =
    currentNodeL.andThen(toScenarioL).mod(this, (s) => s.copy(assertions = s.assertions :+ assertionHolder))
  def configurator(cfg: (P) => Unit) = currentNodeL.andThen(toScenarioL).andThen(configuratorL).mod(this, _ :+ cfg)
  def copyNodes(nodes: List[EngineNode[R, (P) => R]]) = new Builder1[P, R](nodes)
  def build: Engine1[P, R] = BuildEngine.build1(this)
}

trait MakeClosures1[P, R] extends MakeClosures[P, (P) => Boolean, R, (P) => R] {
  def makeBecauseClosure(p: P): BecauseClosure = (bfn) => bfn(p)
  def makeResultClosure(p: P): ResultClosure = (rfn) => rfn(p)
}
trait EvaluateTree1[P, R] extends EvaluateTree[P, (P) => Boolean, R, (P) => R] with Function1[P, R] with MakeClosures1[P, R]
class DecisionTreeLens1[P, R] extends DecisionTreeLens[P, (P) => Boolean, R, (P) => R] {
  def creator(requirements: EngineNodeHolder[R, (P) => R]): (DecisionTreeNode[P, (P) => Boolean, R, (P) => R]) => DecisionTree[P, (P) => Boolean, R, (P) => R] = Builder1.creator(requirements)
}

case class Engine1[P, R](root: DecisionTreeNode[P, (P) => Boolean, R, (P) => R], requirements: EngineNodeHolder[R, (P) => R], rootIsDefault: Boolean = false) extends EngineAndDecisionTree[P, (P) => Boolean, R, (P) => R] with EvaluateTree1[P, R] with Function1[P, R] {
  val lens = new DecisionTreeLens1[P, R]
  def apply(p: P) = evaluate(root, p)
  val expectedToCode: Either[Exception, R] => CodeHolder[(P) => R] = Builder1.expectedToCode[P, R]

}

