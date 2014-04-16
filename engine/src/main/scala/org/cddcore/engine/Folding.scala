package org.cddcore.engine

trait FoldingBuilderNodeAndHolder[R, RFn, FullR] extends BuilderNodeAndHolder[R, RFn] {

}
case class FoldingEngineDescription[R, RFn, FullR](
  val title: Option[String] = None,
  val description: Option[String] = None,
  val code: Option[CodeHolder[RFn]] = None,
  val priority: Option[Int] = None,
  val nodes: List[BuilderNode[R, RFn]] = List(),
  val expected: Option[Either[Exception, R]] = None,
  val references: Set[Reference] = Set(),
  val foldingFn: CodeHolder[(FullR, R) => FullR],
  val initialValue: CodeHolder[() => FullR])
  extends BuilderNodeAndHolder[R, RFn] with FoldingBuilderNodeAndHolder[R, RFn, FullR] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new FoldingEngineDescription[R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new FoldingEngineDescription[R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  def copyNodes(nodes: List[BuilderNode[R, RFn]]) =
    new FoldingEngineDescription[R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  override def toString = s"FoldingEngineDescription(${initialValue.description}, ${foldingFn.description}, nodes=${nodes.mkString(", ")}"
}

//class FoldingBuilderLens[R, RFn, FullR, B <: FoldingBuilder[R, RFn, FullR, B]] {
//
//}
//
//trait FoldingBuilder[R, RFn, FullR, B <: FoldingBuilder[R, RFn, FullR, B]] extends BuilderNodeHolder[R, RFn] {
//  val bl: BuilderLens[R, RFn, FullR, B]
//  import bl._
//  val foldingLens = new FoldingBuilderLens[R, RFn, FullR, B]
//  import foldingLens._
//  protected def wrap(stuff: => Builder[R, RFn, FullR, B]): B
//}

