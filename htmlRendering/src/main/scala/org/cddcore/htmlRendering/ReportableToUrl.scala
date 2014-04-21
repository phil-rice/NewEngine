package org.cddcore.htmlRendering

import org.cddcore.utilities._
import org.cddcore.engine._

class SimpleReportableToUrl extends ReportableToUrl {
  def url(path: List[Reportable])(implicit conv: TemplateLike[Reportable]) = Some("/" + apply(path) + "." + conv(path.head) + ".html")
}
trait ReportableToUrl {
  import Reportable._
  protected var reqId = 0
  protected var cache = Map[Reportable, String]()
  protected var seen = Set[String]()

  def url(path: List[Reportable])(implicit conv: TemplateLike[Reportable]): Option[String]

  protected def findAndAddToCacheIfNeed(r: Reportable)(implicit conv: TemplateLike[Reportable]): String = {
    val existing = cache.get(r)
    val templateName = conv(r)
    existing match {
      case Some(s) => s;
      case _ => {
        def makeNewName: String = {
          reqId += 1; val default = templateName + reqId;
          val result = Strings.urlClean(r match {
            case report: Report => { val result = report.titleOrDescription(default); if (result.length > 120) default else result }
            case project: Project => { val result = project.titleOrDescription(default); if (result.length > 120) default else result }
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
  def apply(path: List[Reportable], separator: String = "/"): String =
    path.reverse.map(apply(_)).mkString(separator)

  import TemplateLike._
  /** We give each reportable a unique id, so that if it occurs once in an html document, we can reference it by id */
  def urlId(r: Reportable, suffix: Option[String] = None)(implicit conv: TemplateLike[Reportable]): String =
    conv(r) + "_" + apply(r) + suffix.collect { case s => "_" + s }.getOrElse("")

  /**
   * this is called from makeUrl. Everything passed to it will have all of it's descendants added. So this should add the item, and then any 'off piste items'. Specifically if
   *  there are any engines in the original path, they need to be added, then added as their requirements and all their child requirements added
   */
  protected def add[Params, BFn, R, RFn, FullR](urlMap: UrlMap, path: List[Reportable]): UrlMap = {
    def justAdd(urlMap: UrlMap, path: List[Reportable]) = { val u = url(path); if (u.isDefined) urlMap + (path -> u.get) else urlMap }
    def addEngine(urlMap: UrlMap, e: Engine[Params, BFn, R, RFn], tail: List[Reportable]) = { //the engine itself has already been added
      val withEd = makeUrlMapForBuilderNodeHolder(e.asRequirement, urlMap, tail) // so this adds the ed and all children under the tail.  
      e match {
        case f: FoldingEngine[Params, BFn, R, RFn, FullR] =>
          val withEnginesAndEd = f.engines.foldLeft(justAdd(withEd, f :: tail))((urlMap, e) => add(urlMap, e :: f :: tail))
          withEnginesAndEd
        case e: Engine[Params, BFn, R, RFn] => withEd
      }
    }
    path match {
      case (e: Engine[Params, BFn, R, RFn]) :: tail => addEngine(justAdd(urlMap, path), e, tail)
      case _ => justAdd(urlMap, path)
    }
  }

  def makeUrlMapFor(r: Reportable, urlMap: UrlMap = UrlMap()): UrlMap =
    add(urlMap, List(r))

  /** This walks down engine descriptions / use cases/ scenarios */
  def makeUrlMapForBuilderNodeHolder[R, RFn](r: BuilderNodeHolder[R, RFn], urlMap: UrlMap = UrlMap(), initialPath: List[Reportable] = List()): UrlMap = {
    val result = r.pathsIncludingSelf(initialPath).foldLeft(urlMap)(add)
    result
  }

  /** This walks down reports/projects/engines and also decision trees => decisions/conclusions. When it gets to an engine, it will then do a second tree of requirements*/
  def makeUrlMap(r: NestedHolder[Reportable], urlMap: UrlMap = UrlMap(), initialPath: List[Reportable] = List()): UrlMap = {
    val fromBasicReportables = r.pathsIncludingSelf(initialPath).foldLeft(urlMap)(add)
    val result = r.all(classOf[Requirement]).foldLeft(fromBasicReportables)((acc, ref) => ref.references.flatMap(_.document).foldLeft(acc)((acc, d) => {
      r match {
        case r: Reportable => add(acc, List(d, r))
        case _ => add(acc, List(d))
      }
    }))
    result
  }

  def makeUrlMapWithDecisionsAndConclusions(r: NestedHolder[Reportable], urlMap: UrlMap = UrlMap()): UrlMap = {
    def add[Params, BFn, R, RFn, FullR](urlMap: UrlMap, path: List[Reportable]): UrlMap = {
      val u = url(path);
      val withU = if (u.isDefined) urlMap + (path -> u.get) else urlMap
      path.head match {
        case f: FoldingEngine[Params, BFn, R, RFn, FullR] =>
          f.engines.foldLeft(urlMap)((acc, e) => add(acc, e :: path))
        case e: EngineFromTests[Params, BFn, R, RFn] =>
          e.tree.pathsFrom[Reportable](path).foldLeft(withU) { add(_, _) }
        case _ => urlMap
      }
    }
    r.pathsIncludingSelf.foldLeft(urlMap) { add(_, _) }
  }
}

