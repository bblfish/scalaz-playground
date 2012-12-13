package oderski

import annotation.tailrec

/**
 *
 */
object Factorial {
   def factorial(n :BigInt): BigInt =
     if (n==0) 1
     else n * factorial(n-1)


   def factorialTr(n: BigInt): BigInt = factorial2(n,List[BigInt]()).fold(BigInt(1))((a1,a2) => a1 * a2)

   @tailrec
   def factorial2(n: BigInt, res: List[BigInt]): List[BigInt] =
     if (n==0) res
     else factorial2(n-1,n::res)

   def sum(f: Int => Int)(a: Int, b: Int): Int = {
     def loop(a: Int, acc: Int): Int = {
       if (a>b) acc
       else loop(a+1,f(a)+acc)
     }
     loop(a,0)
   }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a>b) acc
      else loop(a+1,f(a)*acc)
    }
    loop(a,1)
  }

  def cat(f: Int => Int)(compose: (Int,Int) => Int)(id: Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a>b) acc
      else loop(a+1,compose(f(a),acc))
    }
    loop(a,id)
  }

  import math.abs
  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double) = abs((x-y)/x)/x < tolerance
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess,next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x))/2

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1)

}
