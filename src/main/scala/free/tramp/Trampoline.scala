package free.tramp

import annotation.tailrec
import language.implicitConversions


/**
 * http://skillsmatter.com/podcast/scala/stackless-scala-free-monads
 * A first version of Trampoline to solve the the State and Even running out of stack issues
 */
sealed trait Trampoline[+A] {
//  @tailrec
//  final def runT: A =
//    this match {
//      case More(k) => k().runT
//      case Done(v) => v
//    }
  private case class FlatMap[A,+B]  ( sub: Trampoline[A],
                                      k: A => Trampoline[B]) extends Trampoline[B]


  @tailrec
  final def runT: A = resume match {
    case Right(a) => a
    case Left(k) => k().runT
  }

  @tailrec
  final def resume: Either[() => Trampoline[A], A] = this match {
    case Done(v) => Right(v)
    case More(k) => Left(k)
    case FlatMap(a,f) => a match {
      case Done(v) => f(v).resume
      case More(k) => Left(() => k() flatMap  f)
      case FlatMap(b,g) => {
        b.flatMap((x:Any) => g(x) flatMap f).resume
// as explained in article the danger here is if the left leaning tower of flatMaps is taller than the call stack
//        val fm = FlatMap(b, (x: Any) => FlatMap(g(x), f)): Trampoline[A]
//        fm.resume
      }
    }
  }

  def flatMap[B]( f: A => Trampoline[B]): Trampoline[B] = this match {
    case FlatMap(a, g) => FlatMap(a, (x: Any) => g(x) flatMap f)
    case x => FlatMap(x, f)
  }

  def map[B](f: A => B): Trampoline[B] = flatMap(a => Done(f(a)))

  def zip[B](b: Trampoline[B]): Trampoline[(A,B)] =
    (this.resume, b.resume) match {
      case (Right(a), Right(b)) => Done((a, b))
      case (Left(a), Left(b)) => More(() => a() zip b())
      case (Left(a), Right(b)) => More(() => a() zip Done(b))
      case (Right(a), Left(b)) => More(() => Done(a) zip b())
    }

  /**
   * the size of the trampoline
   * @param i
   * @return
   */
// gets a lot more complicated with flatMap.
//  @tailrec
//  final def size(i: Int=0): Int =
//    this match {
//      case More(k) => k().size(i+1)
//      case Done(v) => i
//      case FlatMap(s,f) => s match {
//        case Done(v) => f(v).size(i+1)
//
//      }
//    }
}

case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

case class Done[+A](result: A) extends Trampoline[A]


object Trampoline {
  // turn something into a trampoline
  implicit def step[A](a: => A): Trampoline[A] = More(() => Done(a))

}