package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c==0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def checkBalance(stack: List[Char], chars: List[Char]): Boolean = {
        if(chars.isEmpty) stack.isEmpty
        else if(chars.head == '(') {
          checkBalance(stack :+ '(', chars.tail)
        }
        else if(chars.head == ')') {
          if(stack.nonEmpty && stack.head == '(') checkBalance(stack.tail, chars.tail)
          else false
        }
        else checkBalance(stack, chars.tail)
      }


      checkBalance(List[Char](), chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def count(coins: List[Int], m: Int, n: Int): Int = {
        if((n >= 1 && m <= 0) || n < 0)  0
        else if(n == 0)  1
        else count(coins, m-1, n) + count(coins, m, n-coins(m-1))
      }

      count(coins, coins.size, money)
    }
  }
