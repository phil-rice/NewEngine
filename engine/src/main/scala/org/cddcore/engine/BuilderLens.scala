package org.cddcore.engine

class BuilderLens[R, RFn, B <: EngineNodeHolder[R, RFn]] {

  val currentNodeL: Lens[B, EngineNode[R, RFn]] = Lens[B, EngineNode[R, RFn]](
    (b: B) => currentNodeForHoldersL.get(b),
    (b: B, n: EngineNode[R, RFn]) => { val result = currentNodeForHoldersL.set(b, n).asInstanceOf[B]; result })

  protected val currentNodeForHoldersL: Lens[EngineNodeHolder[R, RFn], EngineNode[R, RFn]] = Lens[EngineNodeHolder[R, RFn], EngineNode[R, RFn]](
    (b: EngineNodeHolder[R, RFn]) =>
      b.nodes match {
        case (eh: EngineNodeHolder[R, RFn]) :: tail if (!eh.nodes.isEmpty) => currentNodeForHoldersL.get(eh);
        case (n: EngineNode[R, RFn]) :: tail => n
      },
    (enh: EngineNodeHolder[R, RFn], n: EngineNode[R, RFn]) => {
      val result = enh.nodes match {
        case (eh: EngineNodeAndHolder[R, RFn]) :: tail if (!eh.nodes.isEmpty) =>
          enh.copy(nodes = (currentNodeForHoldersL.set(eh, n).asInstanceOf[EngineNode[R, RFn]] :: tail))
        case (_: EngineNode[R, RFn]) :: tail =>
          enh.copy(n :: enh.nodes.tail)
      }
      result
    })

  val nextUseCaseHolderL = Lens[B, EngineNodeHolder[R, RFn]](
    (b) => nextUseCaseHolderForHoldersL.get(b),
    (b, n) => nextUseCaseHolderForHoldersL.set(b, n).asInstanceOf[B])

  protected val nextUseCaseHolderForHoldersL: Lens[EngineNodeHolder[R, RFn], EngineNodeHolder[R, RFn]] = Lens[EngineNodeHolder[R, RFn], EngineNodeHolder[R, RFn]](
    (b: EngineNodeHolder[R, RFn]) => {
      (b, b.nodes) match {
        case (ed: EngineDescription[R, RFn], _) => ed
        case (_, (h: EngineNodeHolder[R, RFn]) :: tail) => nextUseCaseHolderForHoldersL.get(h)
      }
    },
    (enh: EngineNodeHolder[R, RFn], n: EngineNodeHolder[R, RFn]) => {
      (enh, enh.nodes) match {
        case (ed: EngineDescription[R, RFn], _) => n
        case (_, (h: EngineNodeHolder[R, RFn]) :: tail) => enh.copy(nextUseCaseHolderForHoldersL.set(h, n).asInstanceOf[EngineNode[R, RFn]] :: tail)
      }
    })

  val nextScenarioHolderL = Lens[B, EngineNodeHolder[R, RFn]](
    (b) => b,
    (b, n) => throw new IllegalStateException)

  val titleL = Lens[EngineNode[R, RFn], Option[String]](
    (en: EngineNode[R, RFn]) => en.title,
    (en: EngineNode[R, RFn], t: Option[String]) => en.copy(title = t))
  val descriptionL = Lens[EngineNode[R, RFn], Option[String]](
    (en: EngineNode[R, RFn]) => en.description,
    (en: EngineNode[R, RFn], d: Option[String]) => en.copy(description = d))
  val priorityL = Lens[EngineNode[R, RFn], Option[Int]](
    (en: EngineNode[R, RFn]) => en.priority,
    (en: EngineNode[R, RFn], p: Option[Int]) => en.copy(priority = p))
  val nodesL = Lens[EngineNodeHolder[R, RFn], List[EngineNode[R, RFn]]](
    (en: EngineNodeHolder[R, RFn]) => en.nodes,
    (en: EngineNodeHolder[R, RFn], nodes: List[EngineNode[R, RFn]]) => en.copy(nodes))
  def expectedL = Lens[EngineNode[R, RFn], Option[Either[Class[_ <: Exception], R]]](
    (en: EngineNode[R, RFn]) => en.expected,
    (en: EngineNode[R, RFn], ex: Option[Either[Class[_ <: Exception], R]]) => en.copy(expected = ex))

  val codeL = Lens[EngineNode[R, RFn], Option[CodeHolder[RFn]]](
      (b) => b.code, 
      (b, cCodeHolder) => b.copy(code = cCodeHolder))

}
