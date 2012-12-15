package free.article

import language.implicitConversions
import annotation.tailrec


/**
 *
 */
object BinTree {
  type Pa[+A] = (A,A)
  type BinTree[+A] = Free[Pa, A]
  type BTDone[+A] = Done[Pa,A]
  type BTMore[+A] = More[Pa,A]

  implicit def pazFunctor = new scalaz.Functor[Pa] {
    def map[A, B](pair: (A, A))(f: (A) => B): Pa[B] = (f(pair._1),f(pair._2))
  }

  /**
   * turn something into a BinTree
   * warning: do not use in recursive functions ( is this still true ? It was for Trampoline...)
   */
  implicit def step[A](a: => A): BinTree[A] = new BTDone(a)

  //this function can be used in the go(..) function to sum all elements of the tree
  //not a good idea: this turns the go into an infinite loop, perhaps because itself
  // it uses flatmap.
  //  def sum(in: Pa[BinTree[Int]]): BinTree[Int] = {
  //    val (a,b) = in
  //    for {
  //      i <- a
  //      ii <- b
  //    } yield i+ii
  //  }

}