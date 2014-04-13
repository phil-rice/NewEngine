package org.cddcore.engine

trait RequirementLike[R] {
  def title(r: R): Option[String]
  def description(r: R): Option[String]
}

object RequirementOp {
  implicit def toRequirementOp[R: RequirementLike](r: R): RequirementOp[R] = new RequirementOp[R] {
    val F = implicitly[RequirementLike[R]]
    val value = r
  }
}

trait RequirementOp[R] {
  import RequirementLike._
  def value: R
  def titleString(implicit rl: RequirementLike[R]) = rl.title(value).getOrElse(rl.description(value).getOrElse(""))
}

object RequirementLike {

//  implicit object EngineDescriptionAsRequirement extends RequirementLike[EngineDescription] {
//    def title(e: EngineDescription): Option[String] = e.title
//    def description(e: EngineDescription): Option[String] = e.description
//  }
//  implicit object UsecaseAsRequirement extends RequirementLike[UseCase[_]] {
//    def title(e: UseCase[_]): Option[String] = e.title
//    def description(e: UseCase[_]): Option[String] = e.description
//  }
//  implicit object ScenarioAsRequirement extends RequirementLike[Scenario[_, _]] {
//    def title(e: Scenario[_, _]): Option[String] = e.title
//    def description(e: Scenario[_, _]): Option[String] = e.description
//  }

}