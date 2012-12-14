package free

import scala.language.implicitConversions

/**
 * Trampoline using Free
 * from article that goes with  http://skillsmatter.com/podcast/scala/stackless-scala-free-monads
 */
object Trampoline {
  type Trampoline[+A] = Free[Function0 , A]

  /**
   * turn something into a trampoline.
   * warning: do not use in recursive functions.
   */
  implicit def step[A](a: => A): Trampoline[A] = More(() => Done(a))

}
