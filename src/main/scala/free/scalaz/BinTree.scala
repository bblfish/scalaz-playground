package free.scalaz


import language.implicitConversions
import annotation.tailrec
import scalaz.Free


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

  //this function can be used in the go(..) function to sum all elements of the tree
  def sum(in: Pa[BinTree[Int]]): BinTree[Int] = {
    val (a,b) = in
    for {
      i <- a
      ii <- b
    } yield i+ii
  }

}