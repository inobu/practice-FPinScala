package errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def flatMap2[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse() errorhandling.None
  }


  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def fallingFn(i: Int): Int = {
    val y: Int = throw new Exception("make america grate again !")
    try {
      val x = 42 + 5
      x + y
    } catch {
      case
        e: Exception => 43

    }
  }

  def fallingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("make america grate again !")): Int)
    } catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =???

}
