package week3

/**
  * Created by hakanmehmed on 16/06/2018.
  */
object Rationals {
  def main(args: Array[String]): Unit = {
    val x = new Rational(1, 3)
    val y = new Rational(5, 7)
    val z = new Rational(3, 2)

    val result = x.subtract(y).subtract(z)
    println(result)
    println(y.add(y))

    println(x < y)
    println(x max y)
    println(x+y+z)
    //val zero = new Rational(1, 0);
  }
}
