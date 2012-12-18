package free.scalaz


import language.implicitConversions
import annotation.tailrec
import scalaz.{\/-, -\/, Free}


/**
 *
 */
object BinTree {
  import Free.{Return,Suspend}
  type Pa[+A] = (A,A)
  type BinTree[+A] = Free[Pa, A]
  type BTReturn[+A] = Return[Pa,A]
  type BTSuspend[+A] = Suspend[Pa,A]

  implicit def pazFunctor = new scalaz.Functor[Pa] {
    def map[A, B](pair: (A, A))(f: (A) => B): Pa[B] = (f(pair._1),f(pair._2))
  }

  /**
   * turn something into a BinTree
   * warning: do not use in recursive functions ( is this still true ? It was for Trampoline...)
   */
  implicit def step[A](a: => A): BinTree[A] = new BTReturn(a)


  /**
   *   if used in the go function go(..) function of Free to "sum" all elements of the tree,
   *   this will not necessarily end. As shown below it depends on the type of structure
   *   initially.
   *
   *   As shown in sum2 it really replaces the left element ( flatMap ) with it's sum over
   *   the right elements. This would lead to the following transformations
   *
   *   A simple case with 1 left element
   *     B(L(2),B(L(3),L(5)))
   *     B(L(5),L(7))
   *     L(12)
   *
   *  A deeper tree:
   *   B(L(2),B(L(3),B(L(5),L(7))))
   *   B(L(5),B(L(7),L(9)))
   *   B(L(12),L(14))
   *   L(26)
   *
   *   Or the following infinite series if the original tree has two left elements
   *     B(B(L(2),L(3)),B(L(5),L(7)))
   *     B(B(L(7),L(9)),B(L(8),L(10)))
   *     B(B(L(15),L(17)),B(L(17),L(19)))
   *     ...
   *
   *   Or we can have a tree that grows in size at each step...
   *     B( B(B(L(1),L( 2)),B(L(3),L( 5))), B(L(7),L(11)) )
   *     B( B(B(L(8),L(12)),B(L(9),L(13))), B(B(L(10),L(14)),B(L(12),L(16))) )
   *     ...
   *
   *   Since go(...) leads to infinite loops one needs to proceed step by step like this.
   *   > val start = new BTSuspend[Int]((2,new BTSuspend((3,5))))
   *   > toTree(start)
   *   > val a1 = left(start.resume)
   *   > val s1 = sum(a1)
   *   > toTree(s1)
   *   > ...
   */
  def sum(in: Pa[BinTree[Int]]): BinTree[Int] = {
    val (a,b) = in
    for {
      i <- a
      ii <- b
    } yield i+ii
  }

  // same as sum, but written out in terms of map/flatmap, which makes it easier to follow
  def sum2(in: Pa[BinTree[Int]]): BinTree[Int] = {
    val (a,b) = in
    val res = a.flatMap(i => b.map {ii => i+ii})
    res
  }


  //this is not recursive, and should stack overflow on large trees
  def toTree[A](bt: BinTree[A]): BTree[A] = {
    bt.resume match {
      case -\/(pair) => B[A](toTree(pair._1), toTree(pair._2))
      case \/-(value) => L[A](value)
    }
  }

}

trait BTree[T]
case class L[T](leaf: T) extends BTree[T]
case class B[T](left: BTree[T] ,right: BTree[T]) extends BTree[T]