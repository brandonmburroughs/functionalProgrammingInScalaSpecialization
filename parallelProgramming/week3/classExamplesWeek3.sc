import scala.collection.concurrent.TrieMap
import scala.collection.{GenSeq, GenSet, mutable}

/*
 * Lecture 3.1
 */

def initializeArray(xs: Array[Int])(v: Int): Unit = {
  // Parallel for loop relies upon side effect; not very functional
  for (i <- xs.indices.par) {
    xs(i) = v
  }
}


/*
 * Lecture 3.2
 */

def sum(xs: Array[Int]): Int = xs.par.fold(0)(_ + _)


/*
 * Lecture 3.3
 */

def max(xs: Array[Int]): Int = xs.par.fold(Int.MinValue)(math.max)

def isVowel(char: Char): Boolean = {
  val vowels = List('a','e','i','o','u')
  vowels.contains(char.toLower)
}

Array('E', 'P', 'F', 'L', 'A').par.aggregate(0)(
  (count, c) => if (isVowel(c)) count + 1 else count,
  _ + _
)


/*
 * Lecture 3.4
 */

def largestPalindrome(xs: GenSeq[Int]): Int = {
  xs.aggregate(Int.MinValue)(
    (largest, n) => if (n > largest && n.toString == n.toString.reverse) n else largest,
    math.max
  )
}
val array = (0 until 1000000).toArray

largestPalindrome(array)
largestPalindrome(array.par)

import java.util.concurrent._

def intersection(a: GenSet[Int], b: GenSet[Int])= {
  val result = new ConcurrentSkipListSet[Int]()
  for (x <- a) if (b contains x) result.add(x)
  result
}

val sequentialIntersection = intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
val parallelIntersection = intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)

println(s"sequential set size: ${sequentialIntersection.size}, parallel set size: ${parallelIntersection.size}")


def intersectionFilter(a: GenSet[Int], b: GenSet[Int]): GenSet[Int] = {
  if (a.size < b.size) a.filter(b(_))
  else b.filter(a(_))
}

val sequentialIntersectionFilter = intersectionFilter((0 until 1000).toSet, (0 until 1000 by 4).toSet)
val parallelIntersectionFilter = intersectionFilter((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)

val graph = TrieMap[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
graph(graph.size - 1) = 0
val previous = graph.snapshot
for ((k, v) <- graph.par) graph(k) = previous(v)
val violation = graph.find({case (i, v) => v != (i + 2) % graph.size})
println(s"violation: $violation")


/*
 * Lecture 3.5
 */

trait Iterator[T] {
  def hasNext: Boolean

  def next(): T

  def foldLeft[S](z: S)(f: (S, T) => S): S = {
    var result = z
    while (hasNext) result = f(result, next())
    result
  }
}

trait Splitter[A] extends Iterator[A] {
  def split: Seq[Splitter[A]]
  def remaining: Int
}

trait Builder[A, Repr] {
  def +=(elem: A): Builder[A, Repr]
  def result: Repr
}
