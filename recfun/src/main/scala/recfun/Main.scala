package recfun
import common._

object Main {
  def main(args: Array[String]) {
    val numbers = for (row <- 0 to 10) yield {
      for (col <- 0 to row) yield {
        pascal(col, row)
      }
    }

    println(numbers)

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
  def pascal(c: Int, r: Int): Int = if (c == 0 || r == c) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], count: Int): Int = {
      if (chars.isEmpty || count < 0) count
      else {
        if (chars.head == '(') balance(chars.tail, count + 1)
        else if (chars.head == ')') balance(chars.tail, count - 1)
        else balance(chars.tail, count)
      }
    }
  
    balance(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty && money == 0) 1
    else if (coins.isEmpty && money != 0) 0
    else if (money < 0) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
