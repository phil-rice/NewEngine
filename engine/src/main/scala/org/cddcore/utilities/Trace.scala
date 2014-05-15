package org.cddcore.utilities

import org.cddcore.engine.Reportable

object StartChildEndType extends Enumeration {
  type StartChildEndType = Value
  val Start, Child, End = Value
}
import StartChildEndType._


case class TraceItem[Main, Params, Result, Evidence](main: Main, params: Params, result: Either[Exception, Result],
  evidence: Option[Evidence], nodes: List[TraceItem[Main, Params, Result, Evidence]], took: Long,
  textOrder: Int = Reportable.nextTextOrder)
  extends NestedHolder[TraceItem[Main, Params, Result, Evidence]] with Reportable {

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
