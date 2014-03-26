package wickland.ch4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

final case object None extends Option[Nothing] {
  def map[B](f: Nothing => B): Option[B] = None
  def flatMap[B](f: Nothing => Option[B]): Option[B] = None
  def getOrElse[B >: Nothing](default: => B): B = default
  def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
  def filter(f: Nothing => Boolean): Option[Nothing] = None
}

final case class Some[+A](a: A) extends Option[A] {
  def map[B](f: A => B): Option[B] = Some(f(a))
  def flatMap[B](f: A => Option[B]): Option[B] = f(a)
  def getOrElse[B >: A](default: => B): B = a
  def orElse[B >: A](ob: => Option[B]): Option[B] = this
  def filter(f: A => Boolean): Option[A] = if (f(a)) this else None
}
