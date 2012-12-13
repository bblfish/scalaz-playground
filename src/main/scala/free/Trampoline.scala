package free

import annotation.tailrec

/**
 * http://skillsmatter.com/podcast/scala/stackless-scala-free-monads
 * Created with IntelliJ IDEA.
 * User: hjs
 * Date: 12/12/2012
 * Time: 23:03
 * To change this template use File | Settings | File Templates.
 */
sealed trait Trampoline[+A] {
  @tailrec
  final def runT: A =
    this match {
      case More(k) => k().runT
      case Done(v) => v
    }

  /**
   * the size of the trampoline
   * @param i
   * @return
   */
  @tailrec
  final def size(i: Int=0): Int =
    this match {
      case More(k) => k().size(i+1)
      case Done(v) => i
    }
}

case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

case class Done[+A](result: A) extends Trampoline[A]