package org.cddcore.engine

import scala.language.implicitConversions
import scala.reflect.macros.Context
import scala.language.experimental.macros

class BuilderLens1[P, R, B <: EngineNodeHolder[R, (P) => R]] extends BuilderLens[R, (P) => R, B] {
  val becauseL = Lens[EngineNode[R, (P) => R], Option[CodeHolder[(P) => Boolean]]]((b) => None, (b, bCodeHolder) => b)
}

object Builder1 {
  def bl[P, R]() = new BuilderLens1[P, R, Builder1[P, R]]

  def becauseImpl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(because: c.Expr[(P) => Boolean]): c.Expr[Builder1[P, R]] = {
    import c.universe._
    reify {
      val l = bl[P, R]()
      val ch = CodeHolder[(P) => Boolean](because.splice, c.literal(show(because.tree)).splice)
      val thisObject: Builder1[P, R] = (c.Expr[Builder1[P, R]](c.prefix.tree)).splice
      l.currentNodeL.andThen(l.becauseL).set(thisObject, Some(ch))
    }
  }
  def codeImpl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(code: c.Expr[(P) => R]): c.Expr[Builder1[P, R]] = {
    import c.universe._
    reify {
      val l = bl[P, R]()
      val ch = CodeHolder[(P) => R](code.splice, c.literal(show(code.tree)).splice)
      val thisObject: Builder1[P, R] = (c.Expr[Builder1[P, R]](c.prefix.tree)).splice
      l.currentNodeL.andThen(l.codeL).set(thisObject, Some(ch))
    }
  }
  def matchWithImpl[P: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(pf: c.Expr[PartialFunction[P, R]]): c.Expr[Builder1[P, R]] = {
    import c.universe._
    val thisObject: c.Expr[Builder1[P, R]] = (c.Expr[Builder1[P, R]](c.prefix.tree))
    reify {
      val ch = CodeHolder(pf.splice, c.literal(show(pf.tree)).splice)
      thisObject.splice.matchWithPrim(ch)
    }
  }
}

case class Builder1[P, R](nodes: List[EngineNode[R, (P) => R]] = List(new EngineDescription[R, (P) => R])) extends Builder[R, (P) => R, Builder1[P, R]] with BuilderWithModifyChildrenForBuild[R, (P)=>R]{
  val bl1 = Builder1.bl[P, R]()
  import bl1._
  def because(because: (P) => Boolean): Builder1[P, R] = macro Builder1.becauseImpl[P, R]
  def code(code: (P) => R): Builder1[P, R] = macro Builder1.codeImpl[P, R]
 
  def matchWith(pf: PartialFunction[P, R]) = macro Builder1.matchWithImpl[P, R]
  def scenario(p: P, title: String = null) = nextScenarioHolderL.andThen(nodesL).mod(this, (nodes) =>new Scenario[P, (P) => Boolean, R, (P) => R](p, title = Option(title)) :: nodes)
  def matchWithPrim(codeHolder: CodeHolder[PartialFunction[P, R]]) = {
    val withBecause = currentNodeL.andThen(becauseL).set(this, None)
    currentNodeL.andThen(codeL).set(withBecause, None)
  }
  def copyNodes(nodes: List[EngineNode[R, (P) => R]]) = new Builder1[P, R](nodes)
}

trait EvaluateTree1[P, R] extends EvaluateTree[P, (P) => Boolean, R, (P) => R] with Function1[P, R] {
  def makeBecauseClosure(p: P): BecauseClosure = (bfn) => bfn(p)
  def makeResultClosure(p: P): ResultClosure = (rfn) => rfn(p)
}

trait DecisionTree1[P, R] extends DecisionTree[P, (P) => Boolean, R, (P) => R] with EvaluateTree1[P, R] with Function1[P, R] {
  def apply(p: P) = evaluate(root, p)
}

case class Engine1[P, R](root: DecisionTreeNode[P, (P) => Boolean, R, (P) => R]) extends Engine[P, (P) => Boolean, R, (P) => R] with DecisionTree1[P, R];

