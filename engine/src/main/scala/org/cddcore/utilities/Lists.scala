package org.cddcore.utilities

object Lists {

  def increasingList[T](list: List[T]) = {
    list.foldLeft(List[List[T]]())((acc, t) => acc match {
      case h :: tail => (h :+ t) :: acc
      case _ => List(List(t))
    }).reverse
  }
  def decreasingList[T](list: List[T]): List[List[T]] = list match {
    case Nil => Nil;
    case _ => decreasingListPrim(List(list), list).reverse
  }

  private def decreasingListPrim[T](acc: List[List[T]], list: List[T]): List[List[T]] =
    list match {
      case h :: Nil => acc
      case h :: t => decreasingListPrim(t :: acc, t)
      case _ => acc
    }

}