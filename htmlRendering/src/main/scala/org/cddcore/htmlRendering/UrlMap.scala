package org.cddcore.htmlRendering

import org.cddcore.utilities._
import org.cddcore.engine.Reportable

case class UrlMap(val toUrl: KeyedMap[String] = new KeyedMap(), val fromUrl: Map[String, List[Reportable]] = Map()) {
  import KeyLike._
  /** From a reportable to the Url representing it */
  def apply(r: Reportable): String = toUrl(r)
  /** From a reportable to the optional Url representing it */
  def get(r: Reportable): Option[String] = toUrl.get(r)

  /** From a url to the reportable that should be at that url */
  def apply(url: String) = fromUrl(url)
  /** From a url to the optional reportable that should be at that url */
  def get(url: String) = fromUrl.get(url)

  /** Has the reportable got a url? */
  def contains(r: Reportable) = toUrl.contains(r)
  /** Makes a new UrlMap with the path mapping to a url */
  def +(kv: (List[Reportable], String)) = UrlMap(toUrl + (kv._1.head -> kv._2), fromUrl + (kv._2 -> kv._1))
  def size = toUrl.size
}
