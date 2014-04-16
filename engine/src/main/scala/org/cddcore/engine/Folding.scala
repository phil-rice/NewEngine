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
  val foldingFn: (FullR, R) => FullR,
  val initialValue: () => FullR)
  extends BuilderNodeAndHolder[R, RFn] with FoldingBuilderNodeAndHolder[R, RFn, FullR] {
  def copyRequirement(title: Option[String] = title, description: Option[String] = description, priority: Option[Int] = priority, references: Set[Reference] = references) =
    new FoldingEngineDescription[R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  def copyBuilderNode(expected: Option[Either[Exception, R]] = expected, code: Option[CodeHolder[RFn]] = code): BuilderNode[R, RFn] =
    new FoldingEngineDescription[R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)
  def copyNodes(nodes: List[BuilderNode[R, RFn]]) =
    new FoldingEngineDescription[R, RFn, FullR](title, description, code, priority, nodes, expected, references, foldingFn, initialValue)

}

class FoldingBuilderLens[R, RFn, FullR, B <: FoldingBuilder[R, RFn, FullR, B]] {
  val toFoldingEngineDescription = Lens[FoldingBuilder[R, RFn, FullR, B], FoldingEngineDescription[R, RFn, FullR]](
    (b) => b.nodes.head.asInstanceOf[FoldingEngineDescription[R, RFn, FullR]],
    (b, n) => b.copyNodes(nodes = List(n)).asInstanceOf[B],
    Some("toFoldEngine"))
  val foldEngineNodesL = Lens[FoldingEngineDescription[R, RFn, FullR], List[BuilderNode[R, RFn]]](
    (b) => b.nodes,
    (b, n) => b.copyNodes(nodes = n),
    Some("nodesL"))

}

trait FoldingBuilder[R, RFn, FullR, B <: FoldingBuilder[R, RFn, FullR, B]]  extends FoldingBuilderNodeAndHolder[R, RFn, FullR]{
  val foldingLens = new FoldingBuilderLens[R, RFn, FullR, B]
  import foldingLens._
  def childEngine(title: String) =
    toFoldingEngineDescription.andThen(foldEngineNodesL).mod(this, ((n) => new EngineDescription[R, RFn] :: n))
}

