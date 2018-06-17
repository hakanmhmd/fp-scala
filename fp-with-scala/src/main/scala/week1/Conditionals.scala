package week1

import scala.annotation.tailrec

/**
  * Created by hakanmehmed on 14/06/2018.
  */
object Conditionals{
  def loop: Boolean = loop
  def x = loop // evaluates when used
  //val y = loop - evaluates at the point of definition

  def and(x: Boolean, y: Boolean) = {
    // params passed by value
    if(x) y else false
  }

  def andPBN(x:Boolean, y: => Boolean) = {
    if(x) y else false
  }

  def or(x: Boolean, y: Boolean) = {
    if(x) true else y
  }

  def sqrt(x:Double): Double = {
    def abs(num: Double) = {
      if(num > 0) num
      else -num
    }

    def isGoodEnough(guess: Double): Boolean = {
      abs(guess * guess - x) / x < 0.001
    }

    def improve(guess: Double): Double = {
      (guess + x/guess)/2
    }

    def sqrtIter(guess: Double): Double = {
      if(isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    sqrtIter(1.0)
  }

  // tail recursion as the last thing we do is do a recursive call
  @tailrec
  def gcd(x: Int, y: Int): Int = {
    if(y == 0) x
    else gcd(y, x % y)
  }

  def fact(x: Int): Int = {
    if(x == 0) 1
    else fact(x-1) * x
  }

  def factTail(x: Int): Int = {
    def loop(acc: Int,n: Int): Int = {
      if(n == 0) acc
      else loop(acc * n, n-1)
    }
    loop(1, x)
  }


  def main(args: Array[String]): Unit = {
    println(andPBN(false, loop))
    //println(and(false,loop))
    println(or(false, true))
    println(sqrt(0.001))
    println(gcd(14, 21))
    println(fact(4))
    println(factTail(4))
  }
}
