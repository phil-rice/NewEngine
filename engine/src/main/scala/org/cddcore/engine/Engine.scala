package org.cddcore.engine

import java.util.concurrent.atomic.AtomicInteger
import scala.language.implicitConversions
import org.cddcore.utilities.Strings
import org.cddcore.utilities.TraceBuilder
import org.cddcore.utilities.CodeHolder
import org.cddcore.engine.builder._
import org.cddcore.utilities.LoggerDisplayProcessor
import org.cddcore.utilities.ExceptionMap

object TemplateLike {
  implicit object ReportableTemplateLike extends TemplateLike[Reportable] {
    def apply(r: Reportable) = r match {
      case rt: ReportableWithTemplate => rt.template
      case r => r.getClass().getSimpleName
    }
  }
}
trait TemplateLike[T] {
  def apply(t: T): String
}

trait Engine extends Reportable {
  def titleString: String
}

object EngineTools {
  implicit def toEngineTools[Params, BFn, R, RFn](e: Engine) = e.asInstanceOf[EngineTools[Params, BFn, R, RFn]]
}
trait EngineTools[Params, BFn, R, RFn] extends Engine with TypedReportable[Params, BFn, R, RFn] {
  def titleString = asRequirement.titleString
  def asRequirement: EngineRequirement[Params, BFn, R, RFn]
  def evaluator: EvaluateTree[Params, BFn, R, RFn]
  def buildExceptions: ExceptionMap

}

trait EngineRequirement[Params, BFn, R, RFn] extends BuilderNodeAndHolder[Params, BFn, R, RFn] with Requirement with TypedReportable[Params, BFn, R, RFn]{
  def pathsIncludingTree(pathNotIncludingThis: List[Reportable]): List[List[Reportable]] 
}

trait FoldingEngine[Params, BFn, R, RFn, FullR] extends HasExceptionMap[R, RFn] with EngineTools[Params, BFn, R, RFn] {
  def engines: List[EngineFromTests[Params, BFn, R, RFn]]
  def initialValue: CodeHolder[() => FullR]
  def foldingFn: (FullR, R) => FullR
  def applyParams(params: Params): FullR = engines.foldLeft(initialValue.fn())((acc, e) => foldingFn(acc, e.applyParams(params)))
}

trait EngineFromTests[Params, BFn, R, RFn] extends EngineTools[Params, BFn, R, RFn] {
  def tree: DecisionTree[Params, BFn, R, RFn]
  def applyParams(params: Params): R = {
    val monitor = Engine.currentMonitor
    monitor.call(this, params)
    val makeClosures = evaluator.makeClosures
    import makeClosures._
    val c = try {
      val bc = makeBecauseClosure(params)
      evaluator.findConclusion(tree, bc)
    } catch {
      case e: Exception =>
        monitor.failed(this, None, e)
        throw e
    }
    try {
      val result: R = makeResultClosure(params).apply(c.code.fn)
      monitor.finished[R](this, Some(c), result)
      result
    } catch {
      case e: Exception =>
        monitor.failed(this, Some(c), e)
        throw e
    }
  }
  def toString(indent: String, root: DecisionTreeNode[Params, BFn, R, RFn]): String = {
    root match {
      case d: Decision[Params, BFn, R, RFn] =>
        indent + "if(" + d.prettyString + ")\n" +
          toString(indent + " ", d.yes) +
          indent + "else\n" +
          toString(indent + " ", d.no)
      case c: Conclusion[Params, BFn, R, RFn] => indent + c.code.pretty + "\n";
    }
  }
  override def toString(): String = toString("", tree.root)

}

object EngineMonitor {
  def apply() = new NoEngineMonitor

}
trait EngineMonitor {
  def call[Params](e: Engine, params: Params)(implicit ldp: LoggerDisplayProcessor)
  def finished[R](e: Engine, conclusion: Option[Conclusion[_, _, _, _]], result: R)(implicit ldp: LoggerDisplayProcessor)
  def failed(e: Engine, conclusion: Option[Conclusion[_, _, _, _]], exception: Exception)(implicit ldp: LoggerDisplayProcessor)
}

class NoEngineMonitor extends EngineMonitor {
  def call[Params](e: Engine, params: Params)(implicit ldp: LoggerDisplayProcessor) {}
  def finished[R](e: Engine, conclusion: Option[Conclusion[_, _, _, _]], result: R)(implicit ldp: LoggerDisplayProcessor) {}
  def failed(e: Engine, conclusion: Option[Conclusion[_, _, _, _]], exception: Exception)(implicit ldp: LoggerDisplayProcessor) {}
}

class PrintlnEngineMonitor extends EngineMonitor {
  var depth = new AtomicInteger(0)
  private val indent = Strings.blanks(depth.get * 2)
  def call[Params](e: Engine, params: Params)(implicit ldp: LoggerDisplayProcessor) {
    println(Strings.oneLine(s"Calling:  $indent${e.titleString} with ${ldp(params)}"))
    depth.incrementAndGet()
  }
  def finished[R](e: Engine, conclusion: Option[Conclusion[_, _, _, _]], result: R)(implicit ldp: LoggerDisplayProcessor) {
    depth.decrementAndGet()
    println(s"Finished:  $indent ---> ${ldp(result)}")
  }
  def failed(e: Engine, conclusion: Option[Conclusion[_, _, _, _]], exception: Exception)(implicit ldp: LoggerDisplayProcessor) {
    depth.decrementAndGet()
    println(Strings.oneLine(s"Failed:  $indent ---> ${ldp(exception)}"))
  }
}

class TraceEngineMonitor extends EngineMonitor {

  var traceBuilder = TraceBuilder[Engine, Any, Any, Conclusion[_, _, _, _]]()
  def call[Params](e: Engine, params: Params)(implicit ldp: LoggerDisplayProcessor) =
    traceBuilder = traceBuilder.nest(e.asInstanceOf[Engine], params)
  def finished[R](e: Engine, conclusion: Option[Conclusion[_, _, _, _]], result: R)(implicit ldp: LoggerDisplayProcessor) =
    traceBuilder = traceBuilder.finished(result, conclusion)
  def failed(e: Engine, conclusion: Option[Conclusion[_, _, _, _]], exception: Exception)(implicit ldp: LoggerDisplayProcessor) =
    traceBuilder = traceBuilder.failed(exception, conclusion)
  def trace = traceBuilder.children
}

object Engine {
  var logging = false
  /** returns a builder for an engine that implements Function[P,R] */
  def apply[P, R]()(implicit ldp: LoggerDisplayProcessor) = Builder1[P, R, R](BuildEngine.initialNodes, ExceptionMap(), BuildEngine.builderEngine1)(ldp)
  /** returns a builder for an engine that implements Function2[P1,P2,R] */
  def apply[P1, P2, R]()(implicit ldp: LoggerDisplayProcessor) = Builder2[P1, P2, R, R](BuildEngine.initialNodes, ExceptionMap(), BuildEngine.builderEngine2)(ldp)
  /** returns a builder for an engine that implements Function3[P1,P2,P3,R] */
  def apply[P1, P2, P3, R]()(implicit ldp: LoggerDisplayProcessor) = Builder3[P1, P2, P3, R, R](BuildEngine.initialNodes, ExceptionMap(), BuildEngine.builderEngine3)(ldp)

  def folding[P, R, FullR](initialValue: FullR, foldingFn: (FullR, R) => FullR)(implicit ldp: LoggerDisplayProcessor) =
    Builder1[P, R, FullR](BuildEngine.initialNodes[P, (P) => Boolean, R, (P) => R, FullR](initialValue, foldingFn), ExceptionMap(), BuildEngine.folderBuilderEngine1[P, R, FullR])(ldp)
  def foldList[P, R] = folding[P, R, List[R]](List(), (acc: List[R], v: R) => acc :+ v)
  def foldSet[P, R] = folding[P, R, Set[R]](Set(), { _ + _ })

  def folding[P1, P2, R, FullR](initialValue: FullR, foldingFn: (FullR, R) => FullR)(implicit ldp: LoggerDisplayProcessor) =
    Builder2[P1, P2, R, FullR](BuildEngine.initialNodes(initialValue, foldingFn), ExceptionMap(), BuildEngine.folderBuilderEngine2[P1, P2, R, FullR])(ldp)
  def foldList[P1, P2, R] = folding[P1, P2, R, List[R]](List(), (acc: List[R], v: R) => acc :+ v)
  def foldSet[P1, P2, R] = folding[P1, P2, R, Set[R]](Set(), { _ + _ })

  def folding[P1, P2, P3, R, FullR](initialValue: FullR, foldingFn: (FullR, R) => FullR)(implicit ldp: LoggerDisplayProcessor) =
    Builder3[P1, P2, P3, R, FullR](BuildEngine.initialNodes(initialValue, foldingFn), ExceptionMap(), BuildEngine.folderBuilderEngine3[P1, P2, P3, R, FullR])(ldp)
  def foldList[P1, P2, P3, R] = folding[P1, P2, P3, R, List[R]](List(), (acc: List[R], v: R) => acc :+ v)
  def foldSet[P1, P2, P3, R] = folding[P1, P2, P3, R, Set[R]](Set(), { _ + _ })

  def testing = _testing.get
  private var _testing = new ThreadLocal[Boolean] {
    override def initialValue = false;
  }
  def test[X](x: => X) = {
    _testing.set(true)
    try {
      x
    } finally
      _testing.set(false)
  }
  protected val defaultMonitor = new ThreadLocal[EngineMonitor] {
    override def initialValue = EngineMonitor()
  }

  def currentMonitor = defaultMonitor.get

  def withMonitor[X](m: EngineMonitor, fn: => X) = {
    val oldMonitor = defaultMonitor.get
    defaultMonitor.set(m)
    try {
      fn
    } finally {
      defaultMonitor.set(oldMonitor)
    }
  }
  def trace[X](fn: => X) = {
    val tm = new TraceEngineMonitor
    val result = try { Right(withMonitor(tm, fn)) } catch { case e: Exception => Left(e) }
    (result, tm.trace)
  }

}

