package free

import language.implicitConversions


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
   * turn something into a trampoline.
   * warning: do not use in recursive functions.
   */
  implicit def step[A](a: => A): BinTree[A] = new BTDone(a)



}