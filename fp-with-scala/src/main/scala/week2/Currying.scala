package week2

/**
  * Created by hakanmehmed on 16/06/2018.
  */
object Currying {
  def sum(f: Int => Int): (Int, Int) => Int = {
    def innerSum(a: Int, b: Int): Int = {
      if(a > b) 0
      else f(a) + innerSum(a+1, b)
    }
    innerSum
  }

  // multiple parameters list
  // def f(args 1)....(args n)

  // sum2 - function that as input a function and return a function which takes 2 arguments (a,b) and returns an int
  def sum2(f: Int => Int)(a: Int, b: Int): Int = {
    if(a > b) 0
    else f(a) + sum2(f)(a+1, b)
  }

  def prod(f: Int => Int): (Int, Int) => Int = {
    def innerProd(a: Int, b: Int): Int = {
      if(a>b) 1
      else f(a) * innerProd(a+1, b)
    }
    innerProd
  }

  // generalize function for sum and product
  def generalizedFunc(op: (Int, Int) => Int, f: Int => Int, returnVal: Int)(a: Int, b: Int): Int = {
    if(a > b) returnVal
    else op(f(a), generalizedFunc(op, f, returnVal)(a+1, b))
  }

  def fact(x: Int):Int  = prod(x=>x)(1,x) // fact in terms of product
  def cube(x: Int) = x*x*x
  def main(args: Array[String]): Unit = {
    var result = sum(cube)(1, 5)
    println(result)
    result = prod(fact)(1,5)
    println(result)

    val generalResult = generalizedFunc(sum, cube, 0)(1,5)
    println(generalResult)
  }

  def sum(a: Int, b: Int) = a+b
  def prod(a: Int, b: Int) = a*b
}
