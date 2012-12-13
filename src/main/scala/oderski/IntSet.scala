//package oderski
//
//abstract class IntSet {
//  def contains(x: Int): Boolean
//  def incl(x: Int): IntSet
//  def union(other: IntSet): IntSet
//}
//
//object Empty extends IntSet {
//  def contains(x: Int) = false
//
//  def incl(x: Int) = NonEmpty(x, Empty, Empty)
//
//  override def toString = "."
//
//  def union(other: IntSet) = other
//}
//
//case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends  IntSet {
//  def contains(x: Int): Boolean =
//    if (x < elem ) left contains x
//    else right contains x
//    else true
//
//  def incl(x: Int): IntSet =
//    if (x < elem) NonEmpty(elem, left incl x, right)
//    else if ( x > elem) NonEmpty(elem, left, right incl x)
//    else this
//
//  override def toString =  "{"+left+elem+right+"}"
//
//  def union(other: IntSet) =
//     ((left union right) union other) incl elem
//}