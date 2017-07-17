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
    def pascal(c: Int, r: Int): Int = 
      if(c == 0 || r == c) 
        1
      else 
        pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  //   def balance(chars: List[Char]): Boolean = {
  //     def searcher(cs: List[Char], level: Int): Boolean = cs match {
  //       case Nil => level == 0
  //       case ')' :: _ if level < 1 => false
  //       case ')' :: xs => searcher(xs, level - 1)
  //       case '(' :: xs => searcher(xs, level + 1)
  //       case _ :: xs => searcher(xs, level)
  //   }
  //   searcher(chars, 0)
  // }
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = { money match {
        case 0  => 1
        case x if x < 0 => 0
        case x if x>=1 && coins.isEmpty => 0
        case _ => countChange(money, coins.tail) + countChange(money - coins.head, coins)
      }
    }
  }
