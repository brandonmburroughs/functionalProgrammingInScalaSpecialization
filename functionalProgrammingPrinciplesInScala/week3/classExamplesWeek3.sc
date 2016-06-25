import java.util.NoSuchElementException

/*
 * Lecture 3.1
 */

// Abstract classes can exclude method bodies (to be defined)
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def toString = "."
  def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def toString = "{" + left + elem + right + "}"

  def union(other: IntSet): IntSet = ((left union right) union other) incl elem
}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4

val t3 = new NonEmpty(1, Empty, Empty)
t2 union t3

abstract class Base {
  def foo = 1
  def bar: Int
}

class Sub extends Base {
  override def foo = 2
  def bar = 3
}


/*
 * Lecture 3.2
 */

def error(msg: String) = throw new Error(msg)

//error("test")

val x = null
val y: String = x

if (true) 1 else false


/*
 * Lecture 3.3
 */

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

// Pass in type
singleton[Int](1)
singleton[Boolean](true)

// Type inferred
singleton(1)
singleton(true)

def nth[T](n: Int, xs: List[T]): T =
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) xs.head
  else nth(n-1, xs.tail)

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

nth(2, list)
nth(4, list)
nth(-1, list)