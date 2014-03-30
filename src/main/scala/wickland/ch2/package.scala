package wickland

package object ch2 {
  def fib(nth: Long): Long = {
    @annotation.tailrec
    def fib0(stepsRemaining: Long, a: Long, b: Long): Long = {
      if (stepsRemaining <= 0) a
      else fib0(stepsRemaining - 1, b, a + b)
    }
    fib0(nth, 0, 1)
  }

  def isSorted[A](as: Array[A])(gt: (A, A) => Boolean): Boolean = {
    def core(n: Int): Boolean = {
      if (n < 0) true
      else if (!gt(as(n), as(n + 1))) false
      else core(n - 1)
    }
    core(as.length - 2)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    { b => f(a, b) }
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    { a =>
      { b => f(a, b) }
    }
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    { (a, b) => f(a)(b) }
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    { a => f(g(a)) }
  }

  // Chapter 2 from MEAP v8
  //  def absolute(f: Int => Int): Int => Int = { x => math.abs(f(x)) }
  //
  //  def absoluteP[A: Numeric](f: A => A): A => A = { x => implicitly[Numeric[A]].abs(f(x)) }
  //
  //  type Pred[A] = A => Boolean
  //
  //  def divisibleBy(k: Int): Pred[Int] = { x => x % k == 0 }
}
