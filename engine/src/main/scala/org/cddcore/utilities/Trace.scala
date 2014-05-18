package org.cddcore.utilities

import org.cddcore.engine.Reportable
import org.cddcore.engine.Titled

object StartChildEndType extends Enumeration {
  type StartChildEndType = Value
  val Start, Child, End = Value
}
import StartChildEndType._

trait AnyTraceItem extends Reportable {
  def toTraceItem[Main, Params, Result, Evidence] = this.asInstanceOf[TraceItem[Main, Params, Result, Evidence]]
  def toMain[Main] = toTraceItem[Main, Any, Any, Any].main
  def toParams[Params] = toTraceItem[Any, Params, Any, Any].params
  def toEvidence[Main] = toTraceItem[Main, Any, Any, Any].evidence
  def toResult[Result] = toTraceItem[Any, Any, Result, Any].result
  def toNodes[Main, Params, Result, Evidence] = toTraceItem[Main, Params, Result, Evidence].nodes
  def took: Long
  def mainString(implicit ldp: LoggerDisplayProcessor) = toMain[Any] match {
    case titled: Titled => titled.titleString;
    case main => ldp(main)
  }
  def toString(indent: String)(implicit ldp: LoggerDisplayProcessor): String = {
    s"${indent}TraceItem(${mainString(ldp)}, ${ldp(toParams)} => ${ldp(toResult)}\n${toNodes.map((x: AnyTraceItem) => x.toString(ldp, indent + "  "))}"
  }
  def toString(loggerDisplayProcesser: LoggerDisplayProcessor): String = toString(loggerDisplayProcesser, "")
}

object TraceItem {
  def print[Main, Params, Result, Evidence](item: TraceItem[Main, Params, Result, Evidence])(implicit ldp: LoggerDisplayProcessor): String =
    item.paths.foldLeft("")((acc, path) => {
      val i = path.head
      acc + "\n" + Strings.blanks(path.size) + path.head.mainString(ldp) + "(" + ldp(i.params).mkString(",") + ") => " + ldp(i.result) + " ... " + i.took
    })
}

case class TraceItem[Main, Params, Result, Evidence](main: Main, params: Params, result: Either[Exception, Result],
  evidence: Option[Evidence], nodes: List[TraceItem[Main, Params, Result, Evidence]], took: Long,
  textOrder: Int = Reportable.nextTextOrder)
  extends NestedHolder[TraceItem[Main, Params, Result, Evidence]] with AnyTraceItem {

  override def hashCode = main.hashCode() / 2 + params.hashCode / 2
  override def equals(other: Any) =
    other match {
      case t: TraceItem[Main, Params, Result, Evidence] => t.main == main && t.params == params && t.nodes == nodes && t.result == result;
      case _ => false
    }
  override def toString = s"TraceItem($main,$params,$result,children=(${nodes.mkString(",")}))"
}

object TraceBuilder {
  def apply[Main, Params, Result, Evidence](ignore: List[Main] = List()) = new TraceBuilder[Main, Params, Result, Evidence](List(), ignore)
}

class TraceBuilder[Main, Params, Result, Evidence](val children: List[TraceItem[Main, Params, Result, Evidence]], val ignore: List[Main] = List()) {
  val startTime = System.nanoTime()
  def nest(main: Main, params: Params) =
    if (ignore.contains(main)) new IgnoreTraceBuilder(this) else new NestedTraceBuilder[Main, Params, Result, Evidence](main, params, List(), this)
  def copyWithNewItem(item: TraceItem[Main, Params, Result, Evidence]) = new TraceBuilder(children :+ item, ignore)
  def finished(result: Result, evidence: Option[Evidence] = None): TraceBuilder[Main, Params, Result, Evidence] = throw new IllegalStateException;
  def failed(exception: Exception, evidence: Option[Evidence] = None): TraceBuilder[Main, Params, Result, Evidence] = throw new IllegalStateException;
}

class NestedTraceBuilder[Main, Params, Result, Evidence](main: Main, val params: Params, children: List[TraceItem[Main, Params, Result, Evidence]], val parent: TraceBuilder[Main, Params, Result, Evidence]) extends TraceBuilder[Main, Params, Result, Evidence](children, parent.ignore) {
  override def finished(result: Result, evidence: Option[Evidence]) =
    parent.copyWithNewItem(TraceItem(main, params, Right(result), evidence, children, System.nanoTime() - startTime))
  override def failed(exception: Exception, evidence: Option[Evidence]) =
    parent.copyWithNewItem(TraceItem(main, params, Left(exception), evidence, children, System.nanoTime() - startTime))
  override def copyWithNewItem(item: TraceItem[Main, Params, Result, Evidence]) = new NestedTraceBuilder(main, params, children :+ item, parent)
}

class IgnoreTraceBuilder[Main, Params, Result, Evidence](val parent: TraceBuilder[Main, Params, Result, Evidence]) extends TraceBuilder[Main, Params, Result, Evidence](List(), parent.ignore) {
  override def finished(result: Result, evidence: Option[Evidence]) = parent
  override def failed(exception: Exception, evidence: Option[Evidence]) = parent
  override def copyWithNewItem(item: TraceItem[Main, Params, Result, Evidence]) = this
}
