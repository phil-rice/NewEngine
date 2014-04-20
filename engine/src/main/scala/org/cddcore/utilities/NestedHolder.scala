package org.cddcore.utilities

trait NestedHolder[T, +H <: NestedHolder[T, H]] extends Traversable[T] {
  def nodes: List[T]
  def copyNodes(nodes: List[T]): H

  def foreach[U](f: T => U): Unit = {
    for (c <- nodes) c match {
      case holder: NestedHolder[T, H] =>
        f(c); holder.foreach(f)
      case _ => f(c);
    }
  }
  def all[C <: T](clazz: Class[C]) = this.collect { case c: Any if (clazz.isAssignableFrom(c.getClass())) => c.asInstanceOf[C] }

  val paths = new Traversable[List[T]] {
    def foreach[U](f: List[T] => U): Unit =
      foreachPrim[U](nodes, f, List())
    private def foreachPrim[U](nodes: List[T], f: List[T] => U, path: List[T]): Unit = {
      println(s"Path$path Nodes$nodes")
      for (c <- nodes) {
        val cPath = c :: path
        f(cPath)
        c match { case holder: NestedHolder[T, H] => foreachPrim(holder.nodes, f, cPath); case _ => }
      }
    }
  }
}
