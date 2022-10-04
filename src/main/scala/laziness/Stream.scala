package laziness

import laziness.Stream._

import scala.annotation.tailrec

trait Stream[+A] {
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => this
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, _) if p(h()) => p(h())
    case Cons(_, t) => t().exists(p)
    case Cons(_, _) => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def existsViaFold(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def folAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => !p(h()) && !t().folAll(p)
    case Cons(_, _) => true
  }

  def forAllViaFoldRight(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  //  def headOptionViaFoldRight()

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  //間違い
  def append[A2>:A](f: => Stream[A2]): Stream[A2] =
    foldRight(f)((a, b) => b.append(apply(a)))

  //正解
  def append_valid[A2>:A](f: => Stream[A2]): Stream[A2] =
    foldRight(f)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n - 1))
  }

  def fibs(): Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, go(f1, f0 + f1))
    }

    go(0, 1)
  }

  //TODO わからん
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  def fibsViaUnfold(): Stream[Int] = {
    val func: ((Int, Int)) => Option[(Int, (Int, Int))] = {
      case (a, b) =>
        Some(a, (b, a + b))
    }
    unfold((0, 1))(func)
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    val func: Int => Option[(Int, Int)] = a =>
      Some(a, a + 1)
    unfold(n)(func)
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    val func: A => Option[(A, A)] = n =>
      Some(n, n)
    unfold(a)(func)
  }

  def onesViaUnfold(): Stream[Int] =
    unfold(1)(n => Some(1, 1))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, _), 1) => Some(h(), (empty, 0))
    case (Cons(h, t), i) if i > 1 => Some(h(), (t(), i - 1))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(),t())
     case _ => None
  }

  // TODO わからん
  def zipWithViaUnfold[B, C] (s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAllViaUnfold[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some((Some(h1()) -> None) -> (t1() -> Empty))
      case (Empty, Cons(h2, t2)) => Some((None -> Some(h2())) -> (Empty -> t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()) -> Some(h2())) -> (t1() -> t2()))
    }


}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
