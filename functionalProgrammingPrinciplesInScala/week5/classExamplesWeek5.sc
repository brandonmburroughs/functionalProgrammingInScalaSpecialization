/*
 * Lecture 5.1
 */

def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

val xs = List(1,4,56,23)

last(xs)

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

init(xs)

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

concat(List(1,2), List(3,4))

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

reverse(List(1,2,3,4))

def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)

removeAt(1, List('a', 'b', 'c', 'd'))


/*
 * Lecture 5.2
 */

def msortInt(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[Int], ys:List[Int]): List[Int] = {
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    }
    val (fst, snd) = xs splitAt n
    merge(msortInt(fst), msortInt(snd))
  }
}

msortInt(List(12,34,51,5,63,1,9))
msortInt(List(2,-4,5,7,1))


/*
 * Lecture 5.3
 */

import math.Ordering

def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys:List[T]): List[T] = {
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x , y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    }
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

val nums = List(2,-4,5,7,1)
msort(nums)

val fruits = List("apple", "pineapple", "orange", "banana")
msort(fruits)


/*
 * Lecture 5.4
 */

nums map (x => x * 2)

def squareListV(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => y * y :: squareListV(ys)
}

def squareList(xs: List[Int]): List[Int] = xs.map(x => x * x)

squareListV(nums)
squareList(nums)

nums.filter(x => x > 0)
nums.filterNot(x => x > 0)
nums.partition(x => x > 0)

nums.takeWhile(x => x  > 0)
nums.dropWhile(x => x > 0)
nums.span(x => x > 0)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}

val data = List("a", "a", "a", "b", "c", "c", "a")
pack(data)

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs).map(ys => (ys.head, ys.length))
}

encode(data)


/*
 * Lecture 5.5
 */

List(1,2,3,4).reduceLeft(_ + _)
List(1,2,3,4).reduceLeft(_ * _)

List(1,2,3,4).foldLeft(0)(_ + _)
