package free

/**
 * Created with IntelliJ IDEA.
 * User: hjs
 * Date: 12/12/2012
 * Time: 23:02
 * To change this template use File | Settings | File Templates.
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

/**
 * trampolined Even
 */
object EvenT {

  def even[A](ns: List[A]): Trampoline[Boolean] = ns match {
    case Nil => Done(true)
    case x :: xs => More(() => odd(xs))
  }

  def odd[A](ns: List[A]): Trampoline[Boolean] = ns match {
    case Nil => Done(false)
    case x :: xs => More(() => even(xs))
  }
}