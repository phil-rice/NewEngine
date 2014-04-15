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
          enh.copyNodes(nodes = (currentNodeForHoldersL.set(eh, n).asInstanceOf[EngineNode[R, RFn]] :: tail))
        case (_: EngineNode[R, RFn]) :: tail =>
          enh.copyNodes(n :: enh.nodes.tail)
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
        case (_, (h: EngineNodeHolder[R, RFn]) :: tail) => enh.copyNodes(nextUseCaseHolderForHoldersL.set(h, n).asInstanceOf[EngineNode[R, RFn]] :: tail)
      }
    })

  val nextScenarioHolderL = Lens[B, EngineNodeHolder[R, RFn]](
    (b) => nextScenarioHolderForHolderL.get(b),
    (b, n) => nextScenarioHolderForHolderL.set(b, n).asInstanceOf[B])

  protected val nextScenarioHolderForHolderL: Lens[EngineNodeHolder[R, RFn], EngineNodeHolder[R, RFn]] = Lens[EngineNodeHolder[R, RFn], EngineNodeHolder[R, RFn]](
    (b) => b.nodes match {
      case (eh: EngineNodeHolder[R, RFn]) :: tail => nextScenarioHolderForHolderL.get(eh);
      //      case (n: EngineNodeHolder[R, RFn]) :: tail => n
      case _ => b
    },
    (b, n) => {
      val result = b.nodes match {
        case (eh: EngineNodeHolder[R, RFn]) :: tail => b.copyNodes(nodes = nextScenarioHolderForHolderL.set(eh, n).asInstanceOf[EngineNode[R, RFn]] :: tail);
        case _ :: tail => n
        case Nil => n
      }
      result
    })

  val asRequirementL = Lens[EngineNode[R, RFn], Requirement](
    (en) => en,
    (en, t) => t.asInstanceOf[EngineNode[R, RFn]])
  val titleL = Lens.option[Requirement, String](
    (en) => en.title,
    (en, t) => en.copyRequirement(title = t),
    (old, v) => CannotDefineTitleTwiceException(old, v),
    Some("titleL"))
  val descriptionL = Lens.option[Requirement, String](
    (en) => en.description,
    (en, d) => en.copyRequirement(description = d),
    (old, v) => CannotDefineDescriptionTwiceException(old, v),
    Some("descriptionL"))
  val priorityL = Lens.option[Requirement, Int](
    (en) => en.priority,
    (en, p) => en.copyRequirement(priority = p),
    (old, v) => CannotDefinePriorityTwiceException(old, v),
    Some("priorityL"))
  val referencesL = Lens[Requirement, Set[Reference]](
    (en) => en.references,
    (en, references) => en.copyRequirement(references = references))
  val nodesL = Lens[EngineNodeHolder[R, RFn], List[EngineNode[R, RFn]]](
    (en) => en.nodes,
    (en, nodes) => en.copyNodes(nodes))
  val expectedL = Lens.option[EngineNode[R, RFn], Either[Exception, R]](
    (en) => en.expected,
    (en, ex) => en.copyEngineNode(expected = ex),
    (old, v) => CannotDefineExpectedTwiceException(old, v),
    Some("expectedL"))
  def codeL(validate: (EngineNode[R, RFn], EngineNode[R, RFn], CodeHolder[RFn]) => Unit) = Lens.option[EngineNode[R, RFn], CodeHolder[RFn]](
    (b) => b.code,
    (b, cCodeHolder) => b.copyEngineNode(code = cCodeHolder),
    (old, v) => CannotDefineCodeTwiceException(old, v),
    Some("codeL"),
    Some(validate))

}
