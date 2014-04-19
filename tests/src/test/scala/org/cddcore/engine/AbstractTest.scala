package org.cddcore.engine

import org.scalatest._

import bsh.Node
import org.scalatest.FlatSpecLike

trait AssertEquals {
  def assertEquals[T1, T2](expected: T1, actual: T2, prefix: String = "") {
    def msg = prefix + "\nExpected\n" + expected + "\nActual\n" + actual
    if (expected.isInstanceOf[String] & actual.isInstanceOf[String]) {
      val expectedString = expected.asInstanceOf[String];
      val actualString = actual.asInstanceOf[String];
      if (expectedString == actualString)
        return ;
      val s: Traversable[Tuple2[Char, Char]] = for ((ce, ca) <- (expectedString, actualString).zipped) yield (ce, ca)
      for ((t, i) <- s.toList.zipWithIndex) {
        if (t._1 != t._2) {
          val expectedMax = Math.min(i + 10, expectedString.length() - 1)
          val actualMax = Math.min(i + 10, actualString.length() - 1)
          Matchers.fail("First fail at " + i + " Expected: [" + expectedString.substring(i, expectedMax) + "] Actual: [ " + actualString.substring(i, actualMax) + "]\n" + msg)
        }
      }
      expectedString.length() - actualString.length() match {
        case x if x < 0 => Matchers.fail(s"Actual ran over end at ${expectedString.length}\n" + msg)
        case x if x > 0 => Matchers.fail(s"Actual fell short end at ${actualString.length}\n" + msg)
      }

    }
    if (expected != actual)
      assert(expected == actual, msg)
  }

  def assertEquals[T1, T2](prefix: String, expected: T1, actual: T2) {
    assert(expected == actual, prefix + "\nExpected\n" + expected + "\nActual\n" + actual)
  }
}

trait DecisionTreeBuilder[Params, BFn, R, RFn] {
  def expectedToCode: (Either[Exception, R]) => CodeHolder[RFn]
  def scen(params: Params, title: Option[String] = None, description: Option[String] = None, because: Option[CodeHolder[BFn]] = None,
    code: Option[CodeHolder[RFn]] = None, priority: Option[Int] = None, expected: Option[Either[Exception, R]] = None,
    references: Set[Reference] = Set()) =
    new Scenario[Params, BFn, R, RFn](params, title, description, because, code, priority, expected, references)
  def conc(scenario: Scenario[Params, BFn, R, RFn], scenarios: Scenario[Params, BFn, R, RFn]*) =
    new Conclusion[Params, BFn, R, RFn](scenario.actualCode(expectedToCode), List(scenario) ++ scenarios)
  def dec(scenarioThatCausedNode: Scenario[Params, BFn, R, RFn], yes: DecisionTreeNode[Params, BFn, R, RFn], no: DecisionTreeNode[Params, BFn, R, RFn]) =
    new Decision(List(scenarioThatCausedNode.because.get), yes, no, scenarioThatCausedNode)
  def dec(scenariosWithBecause: List[Scenario[Params, BFn, R, RFn]], yes: DecisionTreeNode[Params, BFn, R, RFn], no: DecisionTreeNode[Params, BFn, R, RFn]) =
    new Decision(scenariosWithBecause.map(_.because).collect { case Some(b) => b }, yes, no, scenariosWithBecause.head)
}

trait DecisionTreeBuilderAndBuilderBeingTested[Params, BFn, R, RFn, FullR, B <: Builder[Params, BFn, R, RFn, FullR, B, E], E <: Engine[Params, BFn, R, RFn]]
  extends BuilderTest[Params, BFn, R, RFn, FullR, B, E]
  with BuilderBeingTested[Params, BFn, R, RFn, FullR, B, E]
  with DecisionTreeBuilder[Params, BFn, R, RFn] {
  def s(seed: Seed, title: Option[String] = None, description: Option[String] = None, because: Option[Seed] = None,
    code: Option[CodeHolder[RFn]] = None, priority: Option[Int] = None, expected: Option[ResultSeed] = None,
    references: Set[Reference] = Set()) =
    new Scenario[Params, BFn, R, RFn](params(seed), title, description, because.collect { case seed: Seed => becauseCodeHolder(seed) },
      code, priority, expected.collect { case r: ResultSeed => Right(result(r)) }, references)
  def decisionTreeLens: DecisionTreeLens[Params, BFn, R, RFn]
}

trait AbstractTest extends FlatSpecLike with Matchers with AssertEquals

trait BuilderBeingTested[Params, BFn, R, RFn, FullR, B <: Builder[Params, BFn, R, RFn, FullR, B, E], E <: Engine[Params, BFn, R, RFn]] {
  type Seed
  type ResultSeed
  private var builder: B = initializeBuilder()
  def update(fn: (B) => B): B = { val result = fn(builder); builder = result; result }
  def scenario(seed: Seed, title: String = null) = builder = scenarioImpl(params(seed), title)
  def because(seed: Seed) = builder = becauseImpl(seed)
  def becauseException(e: Exception) = builder = becauseExceptionImpl(e)
  def expected(seed: ResultSeed) = update(_.expected(result(seed)))
  def expectException(e: Exception) = update(_.expectException(e))
  def assertion(callback: => Boolean) = builder = assertionPrim(callback)
  def configurate(cfg: (Params) => Unit) = builder = configuratorPrim(cfg)
  def code(seed: ResultSeed) = update(_.codeHolder(resultCodeHolder(seed)))
  def currentBuilder: B = builder
  def initializeBuilder(nodes: List[BuilderNode[R, RFn]] = List(new EngineDescription[R, RFn])): B
  protected def scenarioImpl(params: Params, title: String): B
  protected def becauseImpl(seed: Seed): B
  protected def becauseExceptionImpl(e: Exception): B
  protected def scenarioObject(params: Params): Scenario[Params, BFn, R, RFn]
  protected def resultCodeHolder(seed: ResultSeed): CodeHolder[RFn]
  protected def becauseCodeHolder(seed: Seed) = CodeHolder[BFn](becauseBfn(seed), s"because$seed")
  protected def assertionPrim(callback: => Boolean): B
  protected def configuratorPrim(cfg: (Params) => Unit): B

  protected def buildImpl(b: B): E;
  def defaultRoot: DecisionTreeNode[Params, BFn, R, RFn]
  def build: E = buildImpl(builder)
  def resetBuilder = builder = initializeBuilder()
  def params(seed: Seed): Params
  def result(seed: ResultSeed): R
  def becauseBfn(seed: Seed): BFn
}

trait BuilderTest[Params, BFn, R, RFn, FullR, B <: Builder[Params, BFn, R, RFn, FullR, B, E], E <: Engine[Params, BFn, R, RFn]]
  extends AbstractTest
  with BeforeAndAfter
  with BuilderBeingTested[Params, BFn, R, RFn, FullR, B, E] {
  type Seed = String
  type ResultSeed = String
  before {
    resetBuilder
  }
  def builderName: String
  val buildEngine: BuildEngine[Params, BFn, R, RFn, FullR, E]
  def decisionTreeLens = buildEngine.decisionTreeLens
  def modifiedChildrenForBuild = buildEngine.builderWithModifyChildrenForBuild.modifyChildrenForBuild(currentBuilder.nodes.head.asInstanceOf[BuilderNodeAndHolder[R, RFn]])

}

trait BuilderConcretizer[From, To] {
  def contains(params: String, because: String) = because.forall(params.contains(_))
  def params(seed: String): From
  def result(seed: String): To
}

trait StringStringTest extends BuilderConcretizer[String, String] {
  def builderName: String = "Builder1[String,String]"
  def params(seed: String): String = seed
  def result(seed: String): String = s"Result($seed)"
  def becauseBfn(seed: String) = (p: String) => contains(p, seed)
}

trait StringStringStringTest extends BuilderConcretizer[(String, String), String] {
  def builderName: String = "Builder2[String,String, String]"
  def params(seed: String) = (seed, seed)
  def result(seed: String) = seed.toString
  def becauseBfn(seed: String) = (p1: String, p2: String) => contains(p1, seed) && contains(p2, seed)
}
trait StringStringStringStringTest extends BuilderConcretizer[(String, String, String), String] {
  def builderName: String = "Builder3[String,String,String,String]"
  def params(seed: String) = (seed, seed, seed)
  def result(seed: String) = seed.toString
  def becauseBfn(seed: String) = (p1: String, p2: String, p3: String) => contains(p1, seed) && contains(p2, seed) && contains(p3, seed)
}

object HolderStringTest {
  val holder = Holder("")
}
trait HolderStringTest extends BuilderConcretizer[Holder, String] {
  import HolderStringTest._
  def builderName: String = "Builder1[Holder,Holder]"
  def params(seed: String): Holder = holder
  def result(seed: String): String = s"Result($seed)"
  def becauseBfn(seed: String) = (p: Holder) => contains(p.value, seed)
}
object HolderHolderStringTest {
  val holderHolder = (Holder(""), Holder(""))
}
trait HolderHolderStringTest extends BuilderConcretizer[(Holder, Holder), String] {
  import HolderHolderStringTest._
  def builderName: String = "Builder2[Holder,Holder, Holder]"
  def params(seed: String) = holderHolder
  def result(seed: String) = seed.toString
  def becauseBfn(seed: String) = (p1: Holder, p2: Holder) => contains(p1.value, seed) && contains(p2.value, seed)
}
object HolderHolderHolderStringTest {
  val holderHolderholder = (Holder(""), Holder(""), Holder(""))
}
trait HolderHolderHolderStringTest extends BuilderConcretizer[(Holder, Holder, Holder), String] {
  import HolderHolderHolderStringTest._
  def builderName: String = "Builder3[Holder,Holder,Holder,Holder]"
  def params(seed: String) = holderHolderholder
  def result(seed: String) = seed.toString
  def becauseBfn(seed: String) = (p1: Holder, p2: Holder, p3: Holder) => contains(p1.value, seed) && contains(p2.value, seed) && contains(p3.value, seed)
}
trait Builder1Test[P, R, FullR]
  extends DecisionTreeBuilderAndBuilderBeingTested[P, (P) => Boolean, R, (P) => R, FullR, Builder1[P, R, FullR], Engine1[P, R, FullR]] {
  def scenarioImpl(p: P, title: String) = update(_.scenario(p, title))
  protected def resultCodeHolder(seed: ResultSeed) = new CodeHolder((p: P) => result(seed), s"(p)=>result$seed")
  protected def scenarioObject(p: P) = new Scenario[P, (P) => Boolean, R, (P) => R](p)
  protected def assertionPrim(callback: => Boolean) = update(_.assertionHolder((p: P, r: Either[Exception, R]) => callback))
  def defaultRoot = BuildEngine.defaultRoot(BuildEngine.defaultRootCode1[P, R])
  protected def becauseImpl(seed: Seed) = update(_.becauseHolder(becauseCodeHolder(seed)))
  protected def becauseExceptionImpl(e: Exception) = update(_.becauseHolder(CodeHolder[(P) => Boolean]((p) => throw e, e.toString())))
  protected def configuratorPrim(cfg: (P) => Unit) = update(_.configurator(cfg))
  def expectedToCode = BuildEngine.expectedToCode1[P, R]
}
trait FoldingBuilderTest[R, FullR] {
  def foldingFn: (FullR, R) => FullR
  def initialValue: FullR
}

trait SimpleBuilder1Test[P, R] extends Builder1Test[P, R, R] {
  lazy val buildEngine = BuildEngine.builderEngine1[P, R]
  def initializeBuilder(nodes: List[BuilderNode[R, (P) => R]] = BuildEngine.initialNodes) = Builder1[P, R, R](nodes, ExceptionMap(), buildEngine)
  protected def buildImpl(b: Builder1[P, R, R]) = b.build
}
trait FoldingBuilder1Test[P, R, FullR] extends Builder1Test[P, R, FullR] with FoldingBuilderTest[R, FullR] {
  lazy val buildEngine = BuildEngine.folderBuilderEngine1[P, R, FullR]
  def initializeBuilder(nodes: List[BuilderNode[R, (P) => R]] = BuildEngine.initialNodes(initialValue, foldingFn)) = Builder1[P, R, FullR](nodes, ExceptionMap(), buildEngine)
  protected def buildImpl(b: Builder1[P, R, FullR]) = b.build
}

trait Builder2Test[P1, P2, R, FullR] extends DecisionTreeBuilderAndBuilderBeingTested[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R, FullR, Builder2[P1, P2, R, FullR], Engine2[P1, P2, R, FullR]] {
  protected def scenarioImpl(params: (P1, P2), title: String) = { val (p1, p2) = params; update(_.scenario(p1, p2, title)) }
  protected def resultCodeHolder(seed: ResultSeed) = new CodeHolder((p1: P1, p2: P2) => result(seed), s"(p1,p2)=>result$seed")
  protected def scenarioObject(p: (P1, P2)) = new Scenario[(P1, P2), (P1, P2) => Boolean, R, (P1, P2) => R](p)
  protected def assertionPrim(callback: => Boolean) = update(_.assertionHolder((params: (P1, P2), r: Either[Exception, R]) => callback))
  def defaultRoot = BuildEngine.defaultRoot(BuildEngine.defaultRootCode2[P1, P2, R])
  protected def configuratorPrim(cfg: ((P1, P2)) => Unit) = update(_.configurator((p1, p2) => cfg(p1, p2)))
  protected def becauseImpl(seed: Seed) = update(_.becauseHolder(becauseCodeHolder(seed)))
  protected def becauseExceptionImpl(e: Exception) = update(_.becauseHolder(CodeHolder[(P1, P2) => Boolean]((p1, p2) => throw e, e.toString())))
  def expectedToCode = BuildEngine.expectedToCode2[P1, P2, R]
}

trait SimpleBuilder2Test[P1, P2, R] extends Builder2Test[P1, P2, R, R] {
  lazy val buildEngine = BuildEngine.builderEngine2[P1, P2, R]
  def initializeBuilder(nodes: List[BuilderNode[R, (P1, P2) => R]] = BuildEngine.initialNodes) = Builder2[P1, P2, R, R](nodes, ExceptionMap(), buildEngine)
  protected def buildImpl(b: Builder2[P1, P2, R, R]) = b.build
}
trait FoldingBuilder2Test[P1, P2, R, FullR] extends Builder2Test[P1, P2, R, FullR] with FoldingBuilderTest[R, FullR] {
  lazy val buildEngine = BuildEngine.folderBuilderEngine2[P1, P2, R, FullR]
  def initializeBuilder(nodes: List[BuilderNode[R, (P1, P2) => R]] = BuildEngine.initialNodes(initialValue, foldingFn)) = Builder2[P1, P2, R, FullR](nodes, ExceptionMap(), buildEngine)
  protected def buildImpl(b: Builder2[P1, P2, R, FullR]) = b.build
}

trait Builder3Test[P1, P2, P3, R, FullR]
  extends DecisionTreeBuilderAndBuilderBeingTested[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R, FullR, Builder3[P1, P2, P3, R, FullR], Engine3[P1, P2, P3, R, FullR]] {
  protected def scenarioImpl(params: (P1, P2, P3), title: String) = { val (p1, p2, p3) = params; update(_.scenario(p1, p2, p3, title)) }
  protected def resultCodeHolder(seed: ResultSeed) = new CodeHolder((p1: P1, p2: P2, p3: P3) => result(seed), s"(p1: P1, p2: P2, p3: P3) => Builder3Test.this.result($seed)")
  protected def scenarioObject(p: (P1, P2, P3)) = new Scenario[(P1, P2, P3), (P1, P2, P3) => Boolean, R, (P1, P2, P3) => R](p)
  protected def assertionPrim(callback: => Boolean) = update(_.assertionHolder((params: (P1, P2, P3), r: Either[Exception, R]) => callback))
  def defaultRoot = BuildEngine.defaultRoot(BuildEngine.defaultRootCode3[P1, P2, P3, R])
  protected def becauseImpl(seed: Seed) = update(_.becauseHolder(becauseCodeHolder(seed)))
  protected def becauseExceptionImpl(e: Exception) = update(_.becauseHolder(CodeHolder[(P1, P2, P3) => Boolean]((p1, p2, p3) => throw e, e.toString())))
  protected def configuratorPrim(cfg: ((P1, P2, P3)) => Unit) = update(_.configurator((p1, p2, p3) => cfg(p1, p2, p3)))
  def expectedToCode = BuildEngine.expectedToCode3[P1, P2, P3, R]
}
trait SimpleBuilder3Test[P1, P2, P3, R] extends Builder3Test[P1, P2, P3, R, R] {
  lazy val buildEngine = BuildEngine.builderEngine3[P1, P2, P3, R]
  def initializeBuilder(nodes: List[BuilderNode[R, (P1, P2, P3) => R]] = BuildEngine.initialNodes) = Builder3[P1, P2, P3, R, R](nodes, ExceptionMap(), buildEngine)
  protected def buildImpl(b: Builder3[P1, P2, P3, R, R]) = b.build
}
trait FoldingBuilder3Test[P1, P2, P3, R, FullR] extends Builder3Test[P1, P2, P3, R, FullR] with FoldingBuilderTest[R, FullR] {
  lazy val buildEngine = BuildEngine.folderBuilderEngine3[P1, P2, P3, R, FullR]
  def initializeBuilder(nodes: List[BuilderNode[R, (P1, P2, P3) => R]] = BuildEngine.initialNodes(initialValue, foldingFn)) = Builder3[P1, P2, P3, R, FullR](nodes, ExceptionMap(), buildEngine)
  protected def buildImpl(b: Builder3[P1, P2, P3, R, FullR]) = b.build
}
