package free.article

import annotation.tailrec
import scalaz.Functor
import scala.language.higherKinds

/**
 * Free Monad,
 * built by abstracting from Trampoline, without using scalaz
 * from the article at http://days2012.scala-lang.org/sites/days2012/files/bjarnason_trampolines.pdf
 */
sealed abstract class Free[S[+_],+A](implicit S: Functor[S]) {
  import scalaz.Liskov.<~<
  import Trampoline._

  private case class FlatMap[S[+ _], A, +B](a: () => Free[S, A],
                                            f: A => Free[S, B])(implicit S: Functor[S]) extends Free[S, B]

  final def resume: Either[S[Free[S, A]], A] =
    this match {
      case Done(a) => Right(a)
      case More(k) => Left(k)
      case a FlatMap f => a() match {
        case Done(a) => f(a).resume
        case More(k) => Left(S.map(k)(_ flatMap f))
        case b FlatMap g => b().flatMap((x: Any) => g(x) flatMap f).resume }
    }

  def flatMap[B]( f: A => Free[S,B]): Free[S,B] = this match {
    case FlatMap(a, g) => FlatMap(a, (x: Any) => g(x) flatMap f)
    case x => FlatMap(() => x, f)
  }

  def map[B](f: A => B): Free[S,B] = flatMap(a => Done(f(a)))

  /** Runs to completion, using a function that extracts the resumption from its suspension functor. */
  final def go[AA >: A](f: S[Free[S, AA]] => Free[S, AA]): AA = {
    @tailrec
    def go2(t: Free[S, AA]): AA = t.resume match {
      case Left(s) => go2(f(s))
      case Right(r) => r
    }
    go2(this)
  }

  /** Runs a trampoline all the way to the end, tail-recursively. */
  def run[B >: A](implicit ev: Free[S, B] <~< Trampoline[B]): B =
    ev(this).go(_())

}

case class Done[S[+ _], +A](a: A)(implicit f: Functor[S]) extends Free[S, A]

case class More[S[+ _], +A](k: S[Free[S, A]])(implicit f: Functor[S]) extends Free[S, A]
