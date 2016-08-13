/*
 * Parallel Definition
 */
import java.util.concurrent._
import scala.util.DynamicVariable


val forkJoinPool = new ForkJoinPool

abstract class TaskScheduler {
  def schedule[T](body: => T): ForkJoinTask[T]
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())
  }
}

class DefaultTaskScheduler extends TaskScheduler {
  def schedule[T](body: => T): ForkJoinTask[T] = {
    val t = new RecursiveTask[T] {
      def compute = body
    }
    Thread.currentThread match {
      case wt: ForkJoinWorkerThread =>
        t.fork()
      case _ =>
        forkJoinPool.execute(t)
    }
    t
  }
}

val scheduler =
  new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

def task[T](body: => T): ForkJoinTask[T] = {
  scheduler.value.schedule(body)
}

def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
  scheduler.value.parallel(taskA, taskB)
}

def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
  val ta = task { taskA }
  val tb = task { taskB }
  val tc = task { taskC }
  val td = taskD
  (ta.join(), tb.join(), tc.join(), td)
}


/*
 * Lecture 2.2
 */

// Arrays
def mapSeq[A,B](lst: List[A], f: A => B): List[B] = lst match {
  case Nil => Nil
  case x :: xs => f(x) :: mapSeq(xs, f)
}

mapSeq(List(1,2,3,4), (x: Int) => x*x)


def mapASegSeq[A,B](in: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {
  var i = left
  while (i < right) {
    out(i) = f(in(i))
    i += 1
  }
}

val in = Array(2,3,4,5,6)
val out = Array(0,0,0,0,0)
val f = (x: Int) => x * x
mapASegSeq(in, 1, 3, f, out)
out

def mapASegPar[A,B](in: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {
  val threshold = 10000

  // Writes to out(i) for left <= i <= right - 1
  if (right - left < threshold) mapASegSeq(in, left, right, f, out)
  else {
    val mid = left + (right - left) / 2
    parallel(mapASegPar(in, left, mid, f, out),
             mapASegPar(in, mid, right, f, out))
  }
}

mapASegPar(in, 0, 5, f, out)
out

// Trees
sealed abstract class Tree[A] {
  val size: Int
}

case class Leaf[A](a: Array[A]) extends Tree[A] {
  override val size = a.size
}

case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {
  override val size = l.size + r.size
}

def mapTreePar[A: Manifest, B: Manifest](t: Tree[A], f: A => B): Tree[B] = {
  t match {
    case Leaf(a) => {
      val len = a.length
      val b = new Array[B](len)
      var i = 0
      while (i < len) {
        b(i) = f(a(i))
        i = i + 1
      }
      Leaf(b)
    }
    case Node(l, r) => {
      val (lb, rb) = parallel(mapTreePar(l, f), mapTreePar(r, f))
      Node(lb, rb)
    }
  }
}
