package week2

/**
  * Created by hakanmehmed on 16/06/2018.
  */
object HigherOrderFunctions {
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    if(a>b) 0
    else f(a) + sum(f, a+1, b)
  }

  def sumInts(a: Int, b: Int) = sum((x: Int) => x, a, b)
  def sumCubes(a: Int, b: Int) = sum(x => x*x*x, a, b)

  def id(x: Int): Int = x
  def cube(x: Int): Int = x*x*x


  def main(args: Array[String]): Unit = {
    println(sumInts(1, 5))

    println(sumTail(id, 1 ,5))
  }

  // tail recursive
  def sumTail(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a>b) acc
      else loop(a+1, f(a) + acc)
    }
    loop(a, 0)
  }
}
