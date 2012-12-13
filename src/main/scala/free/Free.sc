/**
 * video on Free http://skillsmatter.com/podcast/scala/stackless-scala-free-monads
 * and paper on Free http://days2012.scala-lang.org/sites/days2012/files/bjarnason_trampolines.pdf
 */

import free.State
import State._
val p = pureState[Int, List[(Int,Char)]](List())

p.runS(3)

p flatMap( f => f)

