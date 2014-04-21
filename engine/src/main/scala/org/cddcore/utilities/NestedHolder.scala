package org.cddcore.utilities

import org.cddcore.engine.Reportable
import org.cddcore.engine._

trait NestedHolderLike[H, T] {
  def children(h: H): List[T]
  implicit def BuilderNodeAsHolderLike[R, RFn] =
    new NestedHolderLike[BuilderNodeHolder[R, RFn], BuilderNode[R, RFn]] {
      def children(r: BuilderNodeHolder[R, RFn]) = r.nodes
    }
}

trait NestedHolder[T] extends Traversable[T] {
  def nodes: List[T]

  def foreach[U](f: T => U): Unit = {
    for (c <- nodes) c match {
      case holder: NestedHolder[T] =>
        f(c); holder.foreach(f)
      case _ => f(c);
    }
  }
  def all[C <: T](clazz: Class[C]) = this.collect { case c: Any if (clazz.isAssignableFrom(c.getClass())) => c.asInstanceOf[C] }.toList

  abstract class PathTraversable[X >: T](initialPath: List[X]) extends Traversable[List[X]] {

    protected def foreachPrim[U](nodes: List[T], f: List[X] => U, path: List[X]): Unit = {
      println(s"Path$path Nodes$nodes")
      for (c <- nodes) {
        val cPath = c :: path
        f(cPath)
        c match { case holder: NestedHolder[T] => foreachPrim(holder.nodes, f, cPath); case _ => }
      }
    }
  }

  def pathsFrom[X >: T](initialPath: List[X]) = new PathTraversable[X](List()) {
    def foreach[U](f: List[X] => U): Unit = foreachPrim[U](nodes, f, initialPath)
  }
  lazy val paths = pathsFrom[T](List())

  lazy val pathsIncludingSelf: Traversable[List[T]] = pathsIncludingSelf(List())

  def pathsIncludingSelf[X >: T](initialPath: List[X]) = new PathTraversable(initialPath) {
    def foreach[U](f: List[X] => U): Unit = {
      val initialValue = List(NestedHolder.this.asInstanceOf[T])
      f(initialValue)
      foreachPrim[U](nodes, f, initialValue)
    }
  }
}
