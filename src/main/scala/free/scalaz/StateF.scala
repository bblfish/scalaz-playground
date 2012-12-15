package free.scalaz

import scalaz._
import scala.language.higherKinds
import scalaz.Free.Suspend
import scalaz.Free.Return
import annotation.tailrec

/**
 * A StateF following part 6 of the article
 * http://days2012.scala-lang.org/sites/days2012/files/bjarnason_trampolines.pdf
 */
sealed trait StateF[S,+A]

case class Get[S,A](f: S => A)  extends StateF[S,A]
case class Put[S,A](s: S, a: A) extends StateF[S,A]

object StateF {
  // one way to do this
  // private class St[S] {type ate[A] = StateF[S,A] }

  type St[S] = {type λ[+α] = StateF[S,α]}

  type FreeState[S,+A] = Free[St[S]#λ, A]

  implicit def statefFun[S] = new Functor[St[S]#λ] {
      def map[A,B](m: StateF[S, A])(f: A => B): StateF[S, B] = m match {
        case Get(g)    => Get((s:S) => f(g(s)))
        case Put(s, a) => Put(s, f(a))
      }

    }

  def pureState[S,A](a: A): FreeState[S,A] = Return[St[S]#λ,A](a)

  def getState[S]: FreeState[S,S] = Suspend[St[S]#λ, S]( Get(s => Return[St[S]#λ, S](s)))

  // the above is equivalent to the following
  // def getState[S]: FreeState[S,S] = Suspend[St[S]#λ, S]( Get[S,Free[St[S]#λ,S]](s => Return[St[S]#λ, S](s)))

  def setState[S](s: S): FreeState[S,Unit] = Suspend[St[S]#λ, Unit](Put(s,Return[St[S]#λ, Unit](())))

  @tailrec
  def evalS[S,A](s: S, t: FreeState[S,A]): A =
    t.resume match  {
      case -\/(Get(f)) => evalS(s, f(s))
      case -\/(Put(n, a)) => evalS(n, a)
      case \/-(a) => a
    }

  def zipIndex[A](as: List[A]): List[(Int, A)] =
    evalS(0, as.foldLeft( pureState[Int, List[(Int, A)]](List()) ) {
      (acc, a) =>
        for {
          xs <- acc
          n <- getState
          _ <- setState(n + 1)
        } yield (n, a) :: xs
    }).reverse

}