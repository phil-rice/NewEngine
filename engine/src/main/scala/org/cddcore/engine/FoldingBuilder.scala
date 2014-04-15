package org.cddcore.engine

trait FoldingBuilder[R, RFn, FullR, FullRFn, B <: FoldingBuilder[R, RFn, FullR, FullRFn, B]] extends Builder[R, RFn, B] {
  val foldingLens = new FoldingBuilderLens[R, RFn, FullR, FullRFn, B]
  import bl._
  import foldingLens._
  def childEngine(title: String) =
    toFoldingEngineDescription.andThen(foldEngineNodesL).mod(this, ((n) => new EngineDescription[R, RFn] :: n))
}

