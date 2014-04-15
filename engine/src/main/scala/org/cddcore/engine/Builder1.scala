package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros

class BuilderLens1[P, R, B <: BuilderNodeHolder[R, (P) => R]] extends FullBuilderLens[P, (P) => Boolean, R, (P) => R, B]

object Builder1 {
  def bl[P, R]() = new BuilderLens1[P, R, Builder1[P, R]]
  def expectedToCode[P, R]: Either[Exception, R] => CodeHolder[(P) => R] =
    (x) => new CodeHolder((p) => x match { case Right(r) => r }, x.toString())

  def creator[P, R](requirements: BuilderNodeHolder[R, (P) => R]) =
    (r: DecisionTreeNode[P, (P) => Boolean, R, (P) => R],
      buildExceptions: Map[BuilderNode[R, (P) => R], List[Exception]]) => new Engine1(r, requirements, buildExceptions)
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

trait Builder1[P, R] extends Builder[R, (P) => R, Builder1[P, R]]
  with BuilderWithModifyChildrenForBuild[R, (P) => R]
  with ValidateScenario[P, (P) => Boolean, R, (P) => R]
  with MakeClosures1[P, R] {

  val bl1 = Builder1.bl[P, R]()
  import bl1._
  lazy val scenarios = all(classOf[Scenario[P, (P) => Boolean, R, (P) => R]]).toSet
  def because(because: (P) => Boolean): Builder1[P, R] = macro Builder1.becauseImpl[P, R]
  def code(code: (P) => R): Builder1[P, R] = macro Builder1.codeImpl[P, R]

  def becauseHolder(becauseHolder: CodeHolder[P => Boolean]) = wrap(currentNodeL.andThen(toScenarioL).andThen(becauseL((so, sn, b) => checkBecause(sn))).set(this, Some(becauseHolder)))
  def scenario(p: P, title: String = null) = wrap(nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) =>
    checkDuplicateScenario(new Scenario[P, (P) => Boolean, R, (P) => R](p, title = Option(title))) :: nodes))
  def assertionHolder(assertionHolder: CodeHolder[(P, Either[Exception, R]) => Boolean]) =
    wrap(currentNodeL.andThen(toScenarioL).mod(this, (s) => s.copy(assertions = s.assertions :+ assertionHolder)))
  def configurator(cfg: (P) => Unit) = wrap(currentNodeL.andThen(toScenarioL).andThen(configuratorL).mod(this, _ :+ cfg))
  def copyNodes(nodes: List[BuilderNode[R, (P) => R]]) = wrap(new Builder1Class[P, R](nodes, buildExceptions))
  def build: Engine1[P, R] = BuildEngine.build1(this)
  def copyWithNewExceptions(buildExceptions: Map[BuilderNode[R, (P) => R], List[Exception]]) = new Builder1Class[P, R](nodes, buildExceptions)
}

case class Builder1Class[P, R](nodes: List[BuilderNode[R, (P) => R]] = List(new EngineDescription[R, (P) => R]),
  buildExceptions: Map[BuilderNode[R, (P) => R], List[Exception]] = Map[BuilderNode[R, (P) => R], List[Exception]]())(implicit val ldp: LoggerDisplayProcessor)
  extends Builder1[P, R]

trait MakeClosures1[P, R] extends MakeClosures[P, (P) => Boolean, R, (P) => R] {
  def makeBecauseClosure(s: Scenario[P, (P) => Boolean, R, (P) => R]): BecauseClosure = (bfn) => { s.executeConfigurators; bfn(s.params) }
  def makeBecauseClosure(params: P): BecauseClosure = (bfn) => bfn(params)
  def makeResultClosure(s: Scenario[P, (P) => Boolean, R, (P) => R]): ResultClosure = (rfn) => { s.executeConfigurators; rfn(s.params) }
  def makeResultClosure(params: P): ResultClosure = (rfn) => rfn(params)
}
trait EvaluateTree1[P, R] extends EvaluateTree[P, (P) => Boolean, R, (P) => R] with Function1[P, R] with MakeClosures1[P, R]
class DecisionTreeLens1[P, R] extends DecisionTreeLens[P, (P) => Boolean, R, (P) => R] {
  def creator(requirements: BuilderNodeHolder[R, (P) => R]): (DecisionTreeNode[P, (P) => Boolean, R, (P) => R], Map[BuilderNode[R, (P) => R], List[Exception]]) => DecisionTreeAndExceptions[P, (P) => Boolean, R, (P) => R] =
    Builder1.creator(requirements)
}

case class Engine1[P, R](root: DecisionTreeNode[P, (P) => Boolean, R, (P) => R],
  requirements: BuilderNodeHolder[R, (P) => R],
  buildExceptions: Map[BuilderNode[R, (P) => R], List[Exception]],
  rootIsDefault: Boolean = false) extends EngineAndDecisionTree[P, (P) => Boolean, R, (P) => R] with EvaluateTree1[P, R] with Function1[P, R] {
  val lens = new DecisionTreeLens1[P, R]
  def apply(p: P) = evaluate(root, p)
  val expectedToCode: Either[Exception, R] => CodeHolder[(P) => R] = Builder1.expectedToCode[P, R]
}

