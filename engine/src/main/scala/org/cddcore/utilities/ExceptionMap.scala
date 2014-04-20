package org.cddcore.utilities

import org.cddcore.engine.Reportable
import org.cddcore.engine.Requirement
import org.cddcore.engine.Engine
import org.cddcore.engine.Scenario
import scala.annotation.implicitNotFound

object ExceptionMap {
  def apply() = new ExceptionMap()
}
@implicitNotFound("No member of type class ExceptionKeyLike in scope for ${T}")
trait ExceptionKeyLike[-T] {
  def apply(t: T): Int
}
object ExceptionKeyLike {
  implicit object RequirementExceptionKeyLike extends ExceptionKeyLike[Requirement] { def apply(r: Requirement): Int = r.textOrder }
}

class ExceptionMap(val map: Map[Int, List[Exception]] = Map())  {

  def apply[T](t: T)(implicit conv: ExceptionKeyLike[T]) = map(conv(t))
  def +[T](kv: (T, Exception))(implicit conv: ExceptionKeyLike[T]) = kv match {
    case (t, e) => new ExceptionMap(Maps.addToList(map, conv(t), e))
  }
  def ++(em: ExceptionMap) = new ExceptionMap(em.map.foldLeft(map)((acc, kv) => kv._2.foldLeft(acc)((acc, v) => Maps.addToList(acc, kv._1, v))))
  def contains[T](t: T)(implicit conv: ExceptionKeyLike[T]) = map.contains(conv(t))
  def size = map.size
  override def hashCode() = map.hashCode
  override def equals(other: Any) = other match { case e: ExceptionMap => e.map == map; case _ => false }

  def toMap[T, N <: NestedHolder[T, N]](holder: N)(implicit conv: ExceptionKeyLike[T]): Map[T, List[Exception]] = {
    val keyToT = holder.foldLeft(Map[Int, T]())((acc, t) => acc + (conv(t) -> t))
    map.map((kv) => kv match { case (to, le) => keyToT(to) -> le }).foldLeft(Map[T, List[Exception]]()) { _ + _ }
  }
  override def toString = map.toString
} 