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
    if (c == r) 1 else {
      if (c == 0) 1 else
      {
        if ((r - c) <= 1) ((r-1) + 1)
        else
          pascal((c-1), (r-1)) + pascal(c, nonNegativer(r))
      }
    }
  }
  def nonNegativer(r: Int) = if ((r-1) < 0) r else (r-1)

  /**
   * Exercise 2
   */

  def balance(chars: List[Char]): Boolean = {

    def balance(chars: List[Char], openPara: Int): Boolean ={
      if(chars.isEmpty) openPara == 0
      else
      if (chars.head == ')') {
        openPara > 0 && balance(chars.tail, openPara - 1)
      }
      else if (chars.head == '(') {
        balance(chars.tail, openPara + 1)
      }
      else {
        balance(chars.tail, openPara)
      }
    }
    balance(chars, 0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
       def countChange(money:Int, coins: List[Int], count:Int): Int = {
          if (money < 0) count
          else
          if (coins.isEmpty)
            if (money == 0) count + 1 else count
          else
            countChange(money - coins.head, coins, count) + countChange(money, coins.tail, count)
        }
        countChange(money, coins, 0)
      }

  }
