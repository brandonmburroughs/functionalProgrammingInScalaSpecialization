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
    def pascal(c: Int, r: Int): Int = if ((r == 0) || (r == c) || (c == 0)) 1 else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(chars: List[Char], acc: Int): Int = {
      if (acc < 0) {
        acc
      } else if (chars.isEmpty) {
        acc
      } else if (chars.head == '(') {
        balanceHelper(chars.tail, acc + 1)
      } else if (chars.head == ')') {
        balanceHelper(chars.tail, acc - 1)
      } else {
        balanceHelper(chars.tail, acc)
      }
    }

    balanceHelper(chars, 0) == 0
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) {
      0
    } else if (money < 0) {
      0
    } else if (money == 0) {
      1
    } else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
