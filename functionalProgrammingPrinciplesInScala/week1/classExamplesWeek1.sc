import scala.annotation.tailrec

/*
 * Lecture 1.4
 */
// Function that does the same as &&
def and(x: Boolean, y: Boolean): Boolean = if (x) y else false
and(true, true) == true
and(true, false) == false
and(false, true) == false
and(false, false) == false

// Function that does the same as ||
def or(x: Boolean, y: Boolean): Boolean = if (x) true else y
or(true, true) == true
or(true, false) == true
or(false, true) == true
or(false, false) == false

/*
 * Lecture 1.5
 */
1 + 2
def abs(x: Double) = if (x < 0) -x else x

// Square root function with newton's method
def isGoodEnough(guess: Double, x: Double): Boolean =
  abs(guess * guess - x) / x < 0.001

def improve(guess: Double, x: Double): Double =
  (guess + x / guess) / 2

def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

//def sqrt(x: Double) = sqrtIter(1.0, x)

sqrt(2)
sqrt(4)
sqrt(1e-6)
sqrt(1e60)

/*
 * Lecture 1.6
 */

// Square root Newton's method
def sqrt(x: Double): Double = {

  def isGoodEnough(guess: Double): Boolean = abs(guess * guess - x) / x < 0.001

  def improve(guess: Double): Double = (guess + x / guess) / 2

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  sqrtIter(1.0)
}

sqrt(2)


/*
 * Lecture 1.7
 */
@tailrec
def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

gcd(14, 21)

//def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n-1)
def factorial(n: Int): Int = {
  def loop(acc: Int, n: Int): Int = if(n == 0) acc else loop(acc * n, n - 1)

  loop(1, n)
}

factorial(4)

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

countChange(4, List(1, 2))