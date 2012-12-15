package free.article

import scala.language.implicitConversions
import Trampoline._


/**
 * A version of State with the Trampoline based on Free
 */
case class StateT[S,+A](runS: S => Trampoline[(S,A)]) {
  import scalaz.std.function.function0Instance

  def map[B](f: A => B): StateT[S,B] =
    StateT[S, B](s => runS(s) map {
      case (s,a) => (s, f(a))
    })

  def flatMap[B](f: A => StateT[S, B]) =
  // this does not work, creates a steck overflow on zipIndex
  //    StateT[S, B](s => runS(s) flatMap {
  //      case (s1, a) => f(a) runS s1
  //    })
    StateT[S, B](s => More( () => runS(s) flatMap {
      case (s1, a) => f(a) runS s1
    }))


}


object StateT {
  import scalaz.std.function.function0Instance

  def getState[S]: StateT[S,S] = StateT(s => (s,s))
  def setState[S](s: S): StateT[S,Unit] = StateT(_ => (s,()))
  def pureState[S, A](a: A): StateT[S, A] = StateT(s => (s,a))

  def zipIndex[A](as: List[A]): List[(Int,A)] = as.foldLeft(
    pureState[Int, List[(Int,A)]](List())
  )((acc,a) => {
    val res: StateT[Int, List[(Int, A)]] =
      for {
        xs <- acc
        n <- getState
        _ <- setState(n + 1)
      } yield (n, a) :: xs
    res
  }).runS(0).run._2.reverse

  //simplify zipIndex, and show what is happening in terms of flatMaps
  def zipIndex2[A](as: List[A]): StateT[Int,List[(Int,A)]] = as.foldLeft(
    pureState[Int, List[(Int,A)]](List())
  )((acc,a) => {
    val res: StateT[Int, List[(Int, A)]] =
      acc.flatMap{ xsa =>
        getState[Int].flatMap { num =>
          setState[Int](num+1).map { _ =>
            (num, a) :: xsa
          }
        }
      }
    //      for {
    //        xs <- acc
    //        n <- getState
    //        _ <- setState(n + 1)
    //      } yield (n, a) :: xs
    res
  })

  //example of a self-recursive call where step(..) cannot be used
  def fib(n: Int): Trampoline[Int] =
    if (n <= 1) Done(n)
    else for {
      x <- More(() => fib(n - 1))
      y <- More(() => fib(n - 2))
    } yield x + y
}
