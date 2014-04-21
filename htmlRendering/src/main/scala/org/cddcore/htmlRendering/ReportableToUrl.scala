package org.cddcore.htmlRendering

import org.cddcore.utilities._

import org.cddcore.engine._


trait ReportableToUrl {
  import Reportable._
  protected var reqId = 0
  protected var cache = Map[Reportable, String]()
  protected var seen = Set[String]()

  protected def findAndAddToCacheIfNeed(r: Reportable)(implicit conv: TemplateLike[Reportable]): String = {
    val existing = cache.get(r)
    val templateName = conv(r)
    existing match {
      case Some(s) => s;
      case _ => {
        def makeNewName: String = {
          reqId += 1; val default = templateName + reqId;
          val result = Strings.urlClean(r match {
            //            case report: Report => { val result = report.title.getOrElse(default); if (result.length > 120) default else result }
            case project: Project => { val result = project.title.getOrElse(default); if (result.length > 120) default else result }
            case req: Requirement => { val result = req.titleOrDescription(default); if (result.length > 40) default else result }
            case _ => default;
          }).replace(" ", "_")
          if (seen.contains(result)) default else result
        }
        var result: String = null
        do {
          result = makeNewName
        } while (seen.contains(result))
        cache += (r -> result)
        seen += result
        result
      }
    }
  }

  /** Will return a human readable name for the reportable. Will allways return the same name for the reportable */
  def apply(r: Reportable): String = {
    r match {
      case r: ReportableWrapper => findAndAddToCacheIfNeed(r.delegate.getOrElse(r))
      case _ => findAndAddToCacheIfNeed(r)
    }
  }

  /** Will return a human readable name for each reportable in the reversed list. Typically this is used to make a path */
  def apply(path: ReportableList, separator: String = "/"): String = path.reverse.map(apply(_)).mkString(separator)

  /** We give each reportable a unique id, so that if it occurs once in an html document, we can reference it by id */
  def urlId(r: Reportable, suffix: Option[String] = None): String = templateName(r) + "_" + apply(r) + suffix.collect { case s => "_" + s }.getOrElse("")

  def url(path: ReportableList): Option[String]

  protected def add = (urlMap: UrlMap, path: ReportableList) => {
    val u = url(path);
    if (u.isDefined) urlMap + (path -> u.get) else urlMap
  }

  def makeUrlMap(r: ReportableHolder): UrlMap = {
    val fromBasicReportables = r.foldWithPath(UrlMap(Map(), Map()), add)
    documentsIn(r).map(List(_, r)).foldLeft(fromBasicReportables)(add)
  }

  def makeUrlMapWithDecisionsAndConclusions(r: ReportableHolder): UrlMap =
    r.foldWithPath(UrlMap(Map(), Map()), ((acc: UrlMap, path) => {
      def addToMap(acc: UrlMap, path: ReportableList) = {
        val u = url(path);
        val withU = if (u.isDefined) acc + (path -> u.get) else acc
        withU
      }
      val withU = addToMap(acc, path)
      path.head match {
        case e: EngineBuiltFromTests[_] => e.fold(withU, new DecisionTreeFolder[UrlMap] {
          def apply(acc: UrlMap, c: Conclusion) = addToMap(acc, c :: path)
          def apply(acc: UrlMap, d: Decision) = addToMap(acc, d :: path)
        })
        case _ => withU
      }
    }))
}
