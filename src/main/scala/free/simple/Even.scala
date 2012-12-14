package free.simple

import free.tramp.{Done, More, Trampoline}

/**
 * A simple version of even that runs out of stack quickly
 */
object Even {
  def even[A](ns: List[A]): Boolean = ns match {
    case Nil => true
    case x :: xs => odd(xs)
  }
  def odd[A](ns: List[A]): Boolean = ns match {
    case Nil => false
    case x :: xs => even(xs)
  }
}

