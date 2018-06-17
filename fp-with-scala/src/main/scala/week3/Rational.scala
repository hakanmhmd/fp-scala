package week3

/**
  * Created by hakanmehmed on 16/06/2018.
  */
class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non-zero")

  def this(x: Int) = this(x, 1)

  private def gcd(x: Int, y: Int): Int = {
    if(y == 0) x
    else gcd(y, x%y)
  }
  private val g = gcd(x, y)
  def numerator = x / g
  def denominator = y / g

  def add(that: Rational): Rational = {
    new Rational(numerator * that.denominator + that.numerator * denominator,
      denominator * that.denominator)
  }

  def + (that: Rational): Rational = {
    new Rational(numerator * that.denominator + that.numerator * denominator,
      denominator * that.denominator)
  }

  def neg(): Rational = new Rational(-numerator, denominator)
  def unary_-(): Rational = new Rational(-numerator, denominator)

  def subtract(that: Rational): Rational = {
    this + -that //that.neg
  }

  def less(that: Rational): Boolean = {
    numerator * that.denominator < that.numerator * denominator
  }

  def < (that: Rational): Boolean = {
    numerator * that.denominator < that.numerator * denominator
  }

  def max(that: Rational): Rational = {
    if(this < that) that
    else this
  }

  override def toString: String = numerator + "/" + denominator
}
