package free

/**
 * http://skillsmatter.com/podcast/scala/stackless-scala-free-monads
 * @param runS
 * @tparam S
 * @tparam A
 */
case class State[S,+A](runS: S => (S,A)) {
  import State._

  def map[B](f: A => B) =
    State[S, B](s => {
      val (s1,a) = runS(s)
      (s1,f(a))
    })

  def flatMap[B](f: A => State[S,B]) =
    State[S,B](s => {
      val (s1,a) = runS(s)
      f(a) runS s1
    })


}

object State {

  def getState[S]: State[S,S] = State(s => (s,s))
  def setState[S](s: S): State[S,Unit] = State(_ => (s,()))
  def pureState[S, A](a: A): State[S, A] = State(s => (s,a))

  def zipIndex[A](as: List[A]): List[(Int,A)] = as.foldLeft(
    pureState[Int, List[(Int,A)]](List())
  )((acc,a) => {
    val res: State[Int, List[(Int, A)]] =
      for {
        xs <- acc
        n <- getState
        _ <- setState(n + 1)
      } yield (n, a) :: xs
    res
  }).runS(0)._2.reverse

  //simplify zipIndex, and show what is happening in terms of flatMaps
  def zipIndex2[A](as: List[A]): State[Int,List[(Int,A)]] = as.foldLeft(
    pureState[Int, List[(Int,A)]](List())
  )((acc,a) => {
    val res: State[Int, List[(Int, A)]] =
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

}
