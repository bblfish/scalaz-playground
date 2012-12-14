package free


/**
 * trampolined Even
 */
object Even {
  import Trampoline.Trampoline
  import scalaz.std.function.function0Instance


  def even[A](ns: List[A]): Trampoline[Boolean] = ns match {
    case Nil => Done(true)
    case x :: xs => More(() => odd(xs))
  }

  def odd[A](ns: List[A]): Trampoline[Boolean] = ns match {
    case Nil => Done(false)
    case x :: xs => More(() => even(xs))
  }
}

