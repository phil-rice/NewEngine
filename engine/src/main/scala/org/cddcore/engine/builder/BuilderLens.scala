package org.cddcore.engine.builder

import org.cddcore.utilities.Lens
import org.cddcore.engine._
import org.cddcore.utilities.CodeHolder
import org.cddcore.utilities.ExceptionMap

class BuilderLens[R, RFn, FullR, B <: BuilderNodeHolder[R, RFn]] {

  def builderToCanCopyWithNewExceptionMapL = Lens[B, CanCopyWithNewExceptionMap[R, RFn]](
    (b) => b.asInstanceOf[CanCopyWithNewExceptionMap[R, RFn]],
    (b, n) => n.asInstanceOf[B],
    Some("toCanCopyExMapL"))

  val currentNodeL: Lens[B, BuilderNode[R, RFn]] = Lens[B, BuilderNode[R, RFn]](
    (b: B) => currentNodeForHoldersL.get(b),
    (b: B, n: BuilderNode[R, RFn]) => { val result = currentNodeForHoldersL.set(b, n).asInstanceOf[B]; result })

  val exceptionMap = Lens[CanCopyWithNewExceptionMap[R, RFn], ExceptionMap](
    (c) => c.buildExceptions,
    (c, e) =>
      c.copyWithNewExceptions(e),
    Some("exceptionMap"))

  protected val currentNodeForHoldersL: Lens[BuilderNodeHolder[R, RFn], BuilderNode[R, RFn]] = Lens[BuilderNodeHolder[R, RFn], BuilderNode[R, RFn]](
    (b: BuilderNodeHolder[R, RFn]) =>
      b.nodes match {
        case (eh: BuilderNodeHolder[R, RFn]) :: tail if (!eh.nodes.isEmpty) => currentNodeForHoldersL.get(eh);
        case (n: BuilderNode[R, RFn]) :: tail => n
      },
    (enh: BuilderNodeHolder[R, RFn], n: BuilderNode[R, RFn]) => {
      val result = enh.nodes match {
        case (eh: BuilderNodeAndHolder[R, RFn]) :: tail if (!eh.nodes.isEmpty) =>
          enh.copyNodes(nodes = (currentNodeForHoldersL.set(eh, n).asInstanceOf[BuilderNode[R, RFn]] :: tail))
        case (_: BuilderNode[R, RFn]) :: tail =>
          enh.copyNodes(n :: enh.nodes.tail)
      }
      result
    })

  val nextUseCaseHolderL = Lens[B, BuilderNodeHolder[R, RFn]](
    (b) => nextUseCaseHolderForHoldersL.get(b),
    (b, n) => nextUseCaseHolderForHoldersL.set(b, n).asInstanceOf[B])

  val engineDescriptionL: Lens[BuilderNodeHolder[R, RFn], EngineDescription[R, RFn]] = Lens[BuilderNodeHolder[R, RFn], EngineDescription[R, RFn]](
    (b: BuilderNodeHolder[R, RFn]) => {
      (b, b.nodes) match {
        case (ed: EngineDescription[R, RFn], _) => ed
        case (_, (h: BuilderNodeHolder[R, RFn]) :: tail) => engineDescriptionL.get(h)
        case x => throw new IllegalArgumentException(x.toString)
      }
    },
    (enh: BuilderNodeHolder[R, RFn], n: EngineDescription[R, RFn]) => {
      (enh, enh.nodes) match {
        case (ed: EngineDescription[R, RFn], _) => n
        case (_, (h: BuilderNodeHolder[R, RFn]) :: tail) => enh.copyNodes(engineDescriptionL.set(h, n).asInstanceOf[BuilderNode[R, RFn]] :: tail)
        case x => throw new IllegalArgumentException(x.toString)
        
      }
    })

  protected val nextUseCaseHolderForHoldersL: Lens[BuilderNodeHolder[R, RFn], BuilderNodeHolder[R, RFn]] = Lens[BuilderNodeHolder[R, RFn], BuilderNodeHolder[R, RFn]](
    (b: BuilderNodeHolder[R, RFn]) => {
      (b, b.nodes) match {
        case (ed: EngineDescription[R, RFn], _) => ed
        case (_, (h: BuilderNodeHolder[R, RFn]) :: tail) => nextUseCaseHolderForHoldersL.get(h)
        case x => throw new IllegalArgumentException(x.toString)
      }
    },
    (enh: BuilderNodeHolder[R, RFn], n: BuilderNodeHolder[R, RFn]) => {
      (enh, enh.nodes) match {
        case (ed: EngineDescription[R, RFn], _) => n
        case (_, (h: BuilderNodeHolder[R, RFn]) :: tail) => enh.copyNodes(nextUseCaseHolderForHoldersL.set(h, n).asInstanceOf[BuilderNode[R, RFn]] :: tail)
        case x => throw new IllegalArgumentException(x.toString)
      }
    })

  val nodesL = Lens[BuilderNodeHolder[R, RFn], List[BuilderNode[R, RFn]]](
    (b) => b.nodes,
    (b, n) => b.copyNodes(nodes = n),
    Some("nodesL"))

  val nextScenarioHolderL = Lens[B, BuilderNodeHolder[R, RFn]](
    (b) => nextScenarioHolderForHolderL.get(b),
    (b, n) => nextScenarioHolderForHolderL.set(b, n).asInstanceOf[B])

  protected val nextScenarioHolderForHolderL: Lens[BuilderNodeHolder[R, RFn], BuilderNodeHolder[R, RFn]] = Lens[BuilderNodeHolder[R, RFn], BuilderNodeHolder[R, RFn]](
    (b) => b.nodes match {
      case (eh: BuilderNodeHolder[R, RFn]) :: tail => nextScenarioHolderForHolderL.get(eh);
      //      case (n: BuilderNodeHolder[R, RFn]) :: tail => n
      case _ => b
    },
    (b, n) => {
      val result = b.nodes match {
        case (eh: BuilderNodeHolder[R, RFn]) :: tail => b.copyNodes(nodes = nextScenarioHolderForHolderL.set(eh, n).asInstanceOf[BuilderNode[R, RFn]] :: tail);
        case _ :: tail => n
        case Nil => n
      }
      result
    })

  val asRequirementL = Lens[BuilderNode[R, RFn], Requirement](
    (en) => en,
    (en, t) => t.asInstanceOf[BuilderNode[R, RFn]])
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
  val expectedL = Lens.option[BuilderNode[R, RFn], Either[Exception, R]](
    (en) => en.expected,
    (en, ex) => en.copyBuilderNode(expected = ex),
    (old, v) => CannotDefineExpectedTwiceException(old, v),
    Some("expectedL"))
  def codeL(validate: (BuilderNode[R, RFn], BuilderNode[R, RFn], CodeHolder[RFn]) => Unit) = Lens.option[BuilderNode[R, RFn], CodeHolder[RFn]](
    (b) => b.code,
    (b, cCodeHolder) => b.copyBuilderNode(code = cCodeHolder),
    (old, v) => CannotDefineCodeTwiceException(old, v),
    Some("codeL"),
    Some(validate))
  val toFoldingEngineDescription = Lens[B, FoldingEngineDescription[R, RFn, FullR]](
    (b) => b.nodes.head match {
      case f: FoldingEngineDescription[R, RFn, FullR] => f
      case _ => throw new CannotHaveChildEnginesWithoutFolderException
    },
    (b, n) => b.copyNodes(nodes = List(n)).asInstanceOf[B],
    Some("toFoldEngine"))
  val foldEngineNodesL = Lens[FoldingEngineDescription[R, RFn, FullR], List[BuilderNode[R, RFn]]](
    (b) => b.nodes,
    (b, n) => b.copyNodes(nodes = n),
    Some("nodesL"))
}

class FullBuilderLens[Params, BFn, R, RFn, FullR, B <: BuilderNodeHolder[R, RFn]] extends BuilderLens[R, RFn, FullR, B] {
  type S = Scenario[Params, BFn, R, RFn]
  val toScenarioL = Lens[BuilderNode[R, RFn], S](
    (b) => b match { case s: S => s },
    (b, s) => b match { case _: S => s })
  def becauseL(validate: (S, S, CodeHolder[BFn]) => Unit) = Lens.option[S, CodeHolder[BFn]](
    (s) => s.because,
    (s, bCodeHolder) => s.copyScenario(because = bCodeHolder),
    (old, v) => CannotDefineBecauseTwiceException(old, v),
    Some("becauseL"),
    Some(validate))
  def configuratorL = Lens[S, List[Params => Unit]](
    (s) => s.configurators,
    (s, c) => s.copyScenario(configurators = c),
    Some("configuratorL"))
}

