/*
 * Lecture 2.1
 */

//def sumInts(a: Int, b: Int): Int = if (a > b) 0 else a + sumInts(a + 1, b)

//def cube(x: Int): Int = x * x * x

//def sumCubes(a: Int, b: Int): Int = if (a > b) 0 else cube(a) + sumCubes(a+1, b)

// Pass function as parameter
def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f, a + 1, b)

def id(x: Int): Int = x
def cube(x: Int): Int = x * x * x
def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)

//def sumInts(a: Int, b: Int): Int = sum(id, a, b)
//def sumCubes(a: Int, b: Int): Int = sum(cube, a, b)
def sumFactorials(a: Int, b: Int): Int = sum(fact, a, b)

// This can be done more easily with anonymous functions
def sumInts(a: Int, b: Int): Int = sum(x => x, a, b)
def sumCubes(a: Int, b: Int): Int = sum(x => x * x * x, a, b)

// Tail recursive version of sum
def sumTail(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

sumTail(x => x * x)(3, 5)


/*
 * Lecture 2.2
 */

// Currying
def sumCurry(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)
  sumF
}

def sumIntsCurry = sumCurry(x => x)
def sumCubesCurry = sumCurry(x => x * x * x)
def sumFactorialsCurry = sumCurry(fact)

sumCubes(1, 10) + sumFactorials(10, 20)

def sumC(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sumC(f)(a + 1, b)

sumC(cube)(1, 5) == sumCubes(1, 5)

def product(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x * y, 1)(a, b)
  //if (a > b) 1 else f(a) * product(f)(a +1, b)

product(x => x)(1, 5)

def factorial(n: Int): Int = product(x => x)(1, n)
factorial(5)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))


/*
 * Lecture 2.3
 */

def abs(x: Double): Double = if (x > 0) x else - x

val tolerance = 0.0001

def isCloseEnough(x: Double, y: Double): Boolean = abs((x - y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

fixedPoint(x => 1 + x/2)(1)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1.0)

sqrt(2)


/*
 * Lecture 2.5, 2.6, and 2.7
 */

class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must be nonzero!")

  def this(x: Int) = this(x, 1)

  // Greatest common divisor
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)

  // Constructors
  val numer = x / g
  val denom = y / g

  // Methods
  def < (that: Rational): Boolean = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational): Rational = if (this < (that)) that else this

  def + (that: Rational): Rational =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational): Rational = this + -that

  def * (that: Rational): Rational = new Rational(this.numer * that.numer, this.denom * that.denom)

  override def toString = numer + "/" + denom
}

// Object
val x = new Rational(1, 3)
x.numer
x.denom

val y = new Rational(5,7)

val z = new Rational(3, 2)

x - y - z
y - y

x < y
x max y

new Rational(8, 2) * new Rational(1, 2)

