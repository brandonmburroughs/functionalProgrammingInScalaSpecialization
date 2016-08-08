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
 * Lecture 1.6
 */

import scala.util.Random

// Randomly generate points in the quarter circle square
def mcCount(iter: Int): Int = {
  val randomX = new Random
  val randonY = new Random
  var hits = 0
  for (i <- 0 until iter) {
    val x = randomX.nextDouble
    val y = randomX.nextDouble
    if (x * x + y * y < 1) hits = hits + 1
  }
  hits
}

def monteCarloPiSeq(iter: Int): Double =
  4.0 * mcCount(iter) / iter

monteCarloPiSeq(10000)

def monteCarloPiPar(iter: Int): Double = {
  val ((pi1, pi2), (pi3, pi4)) = parallel(
    parallel(mcCount(iter/4), mcCount(iter/4)),
    parallel(mcCount(iter/4), mcCount(iter - 3 * (iter/4)))
  )
  4.0 * (pi1 + pi2 + pi3 + pi4) / iter
}

monteCarloPiPar(10000)

/*
 * Lecture 1.7
 */

def monteCarloPiParTasks(iter: Int): Double = {
  val pi1 = task(mcCount(iter/4))
  val pi2 = task(mcCount(iter/4))
  val pi3 = task(mcCount(iter/4))
  val pi4 = mcCount(iter - 3 * (iter/4))

  4.0 * (pi1.join + pi2.join + pi3.join + pi4) / iter
}

monteCarloPiParTasks(10000)
