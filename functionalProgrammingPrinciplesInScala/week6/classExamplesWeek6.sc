import scala.collection.immutable.IndexedSeq

/*
 * Lecture 6.1
 */

val xs = Array(1, 2, 3, 44)
xs map (x => x * 2)

val s = "Hello World"
s filter (c => c.isUpper)

// Range
1 until 5
1 to 5
1 to 10 by 3
6 to 1 by -2

s exists (c => c.isUpper)
s forall (c => c.isUpper)

val pairs = List(1, 2, 3) zip s
pairs.unzip

s flatMap (c => List('.',c))

xs.sum
xs.max

(1 to 2) flatMap (x => (3 to 5) map (y => (x,y)))

def scalarProductOld(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map{ case (x, y) => x * y}.sum

scalarProductOld(Vector(1,2,3), Vector(1,2,3))

def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)

isPrime(7)
isPrime(8)


/*
 * Lecture 6.2
 */

def pairedPrime(n: Int): IndexedSeq[(Int, Int)] =
  //(1 until n) flatMap (i =>
  //  (1 until i) map (j => (i,j))) filter{case (x, y) => isPrime(x + y)}
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)

pairedPrime(7)

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (for ( (x,y) <- xs zip ys) yield x*y).sum

scalarProductOld(Vector(1,2,3), Vector(1,2,3))


/*
 * Lecture 6.3
 */

val fruitSet = Set("apple", "banana", "pear")
val st = (1 to 6).toSet

// N-Queens problem
def queens(n: Int): Set[List[Int]] = {
  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens

  placeQueens(n)
}

def show(queens: List[Int]): String = {
  val lines =
    for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString

  "\n" + (lines mkString "\n")

}

(queens(8) take 3 map show) mkString


/*
 * Lecture 6.4
 */

val romanNumerals: Map[Char, Int] = Map('I' -> 1, 'V' -> 5, 'X' -> 10)
val capitalOfCountry: Map[String, String] = Map("US" -> "Washington", "Switzerland" -> "Bern")

capitalOfCountry("US")
//capitalOfCountry("Andorra")

capitalOfCountry get "US"
capitalOfCountry get "Andorra"

def showCapital(country: String) = capitalOfCountry get country match {
  case Some(capital) => capital
  case None => "missing data"
}

showCapital("US")
showCapital("Andorra")

val fruit = List("apple", "pear", "orange", "pineapple")
fruit sortWith (_.length < _.length)
fruit.sorted

fruit.groupBy(_.head)


class Poly(val terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0 withDefaultValue 0.0

  //def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
  // foldLeft implementation avoids the extra list creation with (other.terms map adjust)
  def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coef) = term
    terms + (exp -> (coef + terms(exp)))
  }

  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coef) = term
    exp -> (coef + terms(exp))
  }

  override def toString =
    (for ((exp, coef) <- terms.toList.sorted.reverse) yield coef + "x^" + exp) mkString " + "
}

val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
p1 + p2
p1.terms(7)

val cap1 = capitalOfCountry withDefaultValue "<unknown>"
cap1("Andorra")


/*
 * Lecture 6.5
 */

// Assignment:  Translate phone numbers to mnemonics using letters on keys
import scala.io.Source

val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")

val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))

val mnem = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
)

val charCode: Map[Char, Char] =
  for ((digit, keys) <- mnem; letter <- keys) yield letter -> digit

def wordCode(word: String): String = word.toUpperCase map charCode

wordCode("Java")

val wordsForNum: Map[String, Seq[String]] =
  words groupBy wordCode withDefaultValue Seq()

def encode(number: String): Set[List[String]] =
  if (number.isEmpty) Set(List())
  else {
    for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      rest <- encode(number drop split)
    } yield word :: rest
  }.toSet

encode("7225247386")
encode("3187512699")
encode("3183931687")

def translate(number: String): Set[String] = encode(number) map (_ mkString " ")

translate("7225247386")
translate("3187512699")
translate("3183931687")
