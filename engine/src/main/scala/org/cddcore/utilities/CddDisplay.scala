package org.cddcore.utilities

trait CddDisplay {
  def plain(cdp: CddDisplayProcessor): String
  def html(cdp: CddDisplayProcessor): String = plain(cdp)
}

case class ClassFunction[C, T](clazz: Class[C], fn: (CddDisplayProcessor, C) => T) {
  def apply(CddDisplayProcessor: CddDisplayProcessor, c: C) = fn(CddDisplayProcessor, c)
}

case class ClassFunctionList[T](list: List[ClassFunction[_, T]] = List()) {
  def apply[C](ldp: CddDisplayProcessor, c: C) =
    list.collectFirst({ case ClassFunction(clazz, f) if clazz.isAssignableFrom(c.getClass) => f.asInstanceOf[(CddDisplayProcessor, C) => T](ldp, c.asInstanceOf[C]) })
  def getOrElse[C](ldp: CddDisplayProcessor, c: C, default: => T): T =
    apply(ldp, c).getOrElse(default)

}

trait CddDisplayProcessor extends Function[Any, String] {
  def displayMap: ClassFunctionList[String]
  def apply(a: Any): String =
    displayMap.getOrElse(this, a,
      a match {
        case d: CddDisplay => d.plain(this);
        case a => a.toString
      })
  def html(a: Any): String =
    a match {
      case h: CddDisplay => h.html(this)
      case _ => Strings.htmlEscape(a.toString)
    }
}
case class SimpleCddDisplayProcessor(displayMap: ClassFunctionList[String] = ClassFunctionList()) extends CddDisplayProcessor

object CddDisplayProcessor {
  implicit val cdp: CddDisplayProcessor = SimpleCddDisplayProcessor()
  def apply(cf: ClassFunction[_, String]*): CddDisplayProcessor = new SimpleCddDisplayProcessor(ClassFunctionList(cf.toList))
}