package oderski


case class Rational(n: Int, d: Int) {
  require( d != 0, "denominator must be non-zero")

  def this(number: Int) = this(number,1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a %  b)

  private val g = gcd(n,d)

  def numer = n / g
  def denom = d / g

  def < (that: Rational): Boolean = n*that.d <  that.n * denom

  def max(that: Rational) = if ( this < that) that else this


  def +(other: Rational) =
    new Rational(
      n * other.d + other.n * d,
      d * other.d
    )

  def -(that: Rational) = this + -that

  def unary_- = new Rational(-n,d)

  override def toString = numer+"/"+denom

}
