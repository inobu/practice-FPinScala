package datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  //  def product(ds: List[Double]): Double = ds match {
  //    case Nil => 1.0
  //    case Cons(0.0, _) => 0.0
  //    case Cons(x, xs) => x * product(xs)
  //  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  //  def tail[A](cons: Cons[A])(x: A, xs: List[A]): List[A] = cons match {
  //    case Cons(x, xs) => xs
  //    case Cons(x, Nil) => Nil
  //  }


  def tail[B](xs: List[B]): List[B] = xs match {
    case Nil => xs
    case Cons(_, x) => x
  }

  //  def setHead[A](xs: List[A], y: A): List[A] = xs match {
  //    case Nil => throw Exception
  //    case Cons(_, x) => Cons(y, x)
  //  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec def loop[A](k: List[A], m: Int): List[A] = m match {
      case 0 => k
      case a => loop(tail(k), a - 1)
    }

    loop(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    //    case Nil => throw Exception
    case Cons(x, y) => f(x) match {
      case true => dropWhile(y, f)
      case _ => Cons(x, y)
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }


  //  def init[A](l: List[A]): List[A] = l match {
  //    case Nil => l
  //    case Cons(h, t) => Cons(h, Cons(head(t), init(tail(t))))
  //  }

  def head[A](l: List[A]): A = l match {
    case Cons(h, _) => h
  }

  //  def sum(ints : List[Int]): Int = ints match {
  //    case Nil => 0
  //    case Cons(x, xs) => x + sum(xs)
  //  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product((xs))
  }


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)


  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => 1 + y)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec def loop(k: List[A], m: B): B = k match {
      case Nil => m
      case Cons(x, xs) => loop(xs, f(m, x))
    }

    loop(as, z)
  }

  def sum3(as: List[Int]) =
    foldLeft(as, 0)(_ + _)

  def product3(as: List[Double]) =
    foldLeft(as, 1.0)(_ * _)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((x, _) => 1 + x)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((acc, y) => Cons(y, acc))

  def foldLeft2[A, B](as: List[A], acc: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(acc)

  def foldRight2[A, B](as: List[A], acc: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), acc)((a, b) => f(b, a))

  def foldRight3[A, B](as: List[A], acc: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(acc)

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, y) => Cons(x, y))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])((x, y) => append(x, y))

  def mapPlusOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((x, y) => Cons(x + 1, y))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((x, y) => Cons(x.toString, y))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((x, y) => Cons(f(x), y))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((x, y) => f(x) match {
      case true => Cons(x, y)
      case false => y
    })

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((x, y) => append(f(x), y))

  def flatMap2[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => f(x) match {
      case true => List(x)
      case false => Nil
    })

  def aaaaa(l: List[Int], m: List[Int]): List[Int] =
    List(head(l) + head(m))

//  def zipWith[A, B, C](l: List[A], m: List[B])(f: (A, B) => C): List[C] = (l, m) match {
//    case Cons(Nil, _) => Nil
//    case Cons(_, Nil) => Nil
//    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
//  }


}
