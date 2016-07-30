package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("minimum of 2 elements in heap is smaller") = forAll {(a: A, b: A) =>
    val h = insert(b, insert(a, empty))
    val minElement = Math.min(a, b)
    findMin(h) == minElement
  }

  property("removing element from 1 element heap returns empty") = forAll {(a: A, h: H) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  def sortHeap(seq: Seq[A], heap: H): Seq[A] = {
    if (isEmpty(heap)) seq
    else sortHeap(seq :+ findMin(heap), deleteMin(heap))
  }

  property("findMin and deleteMin yield a sorted sequence of elements in heap") = forAll { h: H =>
    val sortedHeap = sortHeap(List(), h)
    sortedHeap == sortedHeap.sorted
  }

  property("minimum of two heaps melded") = forAll {(h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("melding two heaps yields all of the elements of a heap") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val h1Sorted = sortHeap(List(), h1)
    val h2Sorted = sortHeap(List(), h2)
    val hSorted = sortHeap(List(), h)
    (h1Sorted ++ h2Sorted).toSet == hSorted.toSet
  }

}
