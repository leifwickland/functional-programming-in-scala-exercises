package wickland

package object ch4 {
  def optionFromDouble(d: Double): Option[Double] = {
    if (d.isInfinite || d.isNaN) None
    else Some(d)

  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else optionFromDouble(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { m =>
      val variances = xs.map(x => math.pow(x - m, 2))
      mean(variances)
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      someA <- a
      someB <- b
    } yield f(someA, someB)
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverseTailRec(a)(identity)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case x :: xs => for {
        b <- f(x)
        bs <- traverse(xs)(f)
      } yield b :: bs
    }
  }

  def traverseTailRec[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @annotation.tailrec
    def loop(more: List[A], revAcc: List[B]): Option[List[B]] = more match {
      case Nil => Some(revAcc.reverse)
      case x :: xs => f(x) match {
        case None => None
        case Some(b) => loop(xs, b :: revAcc)
      }
    }
    loop(a, Nil)
  }
}
