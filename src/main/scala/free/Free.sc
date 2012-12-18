/**
 * video on Free http://skillsmatter.com/podcast/scala/stackless-scala-free-monads
 * and paper on Free http://days2012.scala-lang.org/sites/days2012/files/bjarnason_trampolines.pdf
 */


import free.article.BinTree._
import free.article.FlatMap

val m = new BTMore[Int]((new BTMore((2,5)),new BTDone(3)))

val fm = m.flatMap(x=>if (x%2==0) new BTDone(x+2) else new BTMore((new BTDone(x+3), new BTDone(x+5))))

val FM = fm.asInstanceOf[FlatMap[Pa,Int,Int]]

import free.scalaz.BinTree._
val start = new BTSuspend[Int]((new BTSuspend((new BTSuspend((1,2)),new BTSuspend((3,5)))),new BTSuspend((7,11))))