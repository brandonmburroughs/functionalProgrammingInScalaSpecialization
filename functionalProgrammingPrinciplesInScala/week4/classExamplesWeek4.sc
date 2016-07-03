/*
 * Lecture 4.1
 */

// Boolean
/*
abstract class Boolean {
  def ifThenElse[T](t: => T, e: => T): T

  def && (x: => Boolean): Boolean = ifThenElse(x, false)
  def || (x: => Boolean): Boolean = ifThenElse(true, x)
  def unary_!: Boolean = ifThenElse(false, true)

  def == (x: Boolean): Boolean = ifThenElse(x, x.unary_!)
  def != (x: Boolean): Boolean = ifThenElse(x.unary_!, x)

  def < (x: Boolean): Boolean = ifThenElse(false, x)
}

object true extends Boolean {
  def ifThenElse[T](t: => T, e: => T) = t
}

object false extends Boolean {
  def ifThenElse[T](t: => T, e: => T) = e
}
*/

// Natural Numbers (Peano numbers)
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = throw new Error("0 has no precessor")
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if (that.isZero) this else throw new Error("negative number")
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def + (that: Nat): Nat = new Succ(n + that)
  def - (that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}

val t = Zero.successor
val s = new Succ(Zero) + t

/*
 * Lecture 4.2 and 4.4
 */
/*
trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List {
  def apply[T]() = Nil
  def apply[T](elem1: T) = new Cons[T](elem1, Nil)
  def apply[T](elem1: T, elem2: T) = new Cons(elem1, new Cons(elem2, Nil))
}

List()
List(1)
List(2,3)

object test {
  val x: List[String] = Nil
}
*/
/*
 * Lecture 4.5
 */

// Classification and access methods
// Not great, too many methods
/*
trait Expr {
  def isNumber: Boolean
  def isSum: Boolean
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr
}

class Number(n: Int) extends Expr {
  def isNumber: Boolean = true
  def isSum: Boolean = false
  def numValue: Int = n
  def leftOp: Expr = throw new Error("Number.leftOp")
  def rightOp: Expr = throw new Error("Number.rightOp")
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  def isNumber: Boolean = false
  def isSum: Boolean = true
  def numValue: Int = throw new Error ("Sum.numValue")
  def leftOp: Expr = e1
  def rightOp: Expr = e2
}

def eval(e: Expr): Int = {
  if (e.isNumber) e.numValue
  else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
  else throw new Error ("Unknown expression" + e)
}

eval(new Sum(new Sum(new Number(1), new Number(2)), new Number(3)))
*/

// Object-oriented decomposition
// This is great until you need to add new methods; need to touch all classes
/*
trait Expr {
  def eval: Int
}

class Number(n: Int) extends Expr {
  def eval: Int = n
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  def eval: Int = e1.eval + e2.eval
}
*/


/*
 * Lecture 4.6
 */

trait Expr

// Case classes implicitly define companion objects with factory methods
case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

case class Var(x: String) extends Expr

def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
  case Prod(e1, e2) => eval(e1) * eval(e2)
}

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Sum(e1, e2) => show(e1) + " + " + show(e2)
  case Prod(e1, e2) => (e1, e2) match {
    case (e1: Sum, e2: Sum) => "(" + show(e1) + ")" + " * " + "(" + show(e2) + ")"
    case (e1: Sum, e2) => "(" + show(e1) + ")" + " * " + show(e2)
    case (e1, e2: Sum) => show(e1) + " * " + "(" + show(e2) + ")"
    case (e1, e2) => show(e1) + " * " + show(e2)
  }
  case Var(x) => x
}

// Eval
eval(Sum(Number(5), Number(4)))
eval(Prod(Number(5), Number(4)))

// Show
show(Prod(Sum(Number(5), Number(4)), Number(2)))
show(Prod(Number(5), Number(4)))

// Exercise
show(Sum(Prod(Number(2), Var("x")), Var("y")))
show(Prod(Sum(Number(2), Var("x")), Var("y")))
show(Prod(Sum(Number(2), Var("x")), Sum(Var("y"), Number(3))))

/*
 * Lecture 4.7
 */

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}

isort(List(24,12,56,34,345,76))