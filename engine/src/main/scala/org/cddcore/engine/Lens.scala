package org.cddcore.engine

trait LensTrait[A, B] extends Immutable {
  def get: A => B
  def set: (A, B) => A
  def apply(whole: A): B = get(whole)
  def mod(a: A, f: B => B): A = set(a, f(get(a)))
  def description: Option[String]
}
case class Lens[A, B](get: A => B, set: (A, B) => A, description: Option[String] = None) extends LensTrait[A, B] {
  def compose[C](that: LensTrait[C, A]) = Lens[C, B](
    c => get(that.get(c)),
    (c, b) => that.mod(c, set(_, b)), Some(that.description.getOrElse("<?>") + "." + description.getOrElse("<?>")))
  def andThen[C](that: Lens[B, C]) = that compose this
  override def toString = description.collect { case d: String => s"Lens($d)" }.getOrElse(super.toString)
}

