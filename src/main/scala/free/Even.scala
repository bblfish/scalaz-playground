package free


/**
 * trampolined Even
 */
object EvenT {
  import Trampoline.Trampoline

  def even[A](ns: List[A]): Trampoline[Boolean] = ns match {
    case Nil => Done(true)
    case x :: xs => More(() => odd(xs))
  }

  def odd[A](ns: List[A]): Trampoline[Boolean] = ns match {
    case Nil => Done(false)
    case x :: xs => More(() => even(xs))
  }
}

