package free

import scala.language.higherKinds

/**
 * a simple version of Functor, just what is needed for the examples here
 */
trait Functor[F[_]] {
  def map[A,B](m: F[A])(f: A => B): F[B]
}

object Functor {

  implicit val f0Functor = new Functor[Function0] {
      def map[A,B](a: () => A)(f: A => B) = () => f(a())
  }
}