package datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](l: Tree[A]): Int = l match {
    case Branch(a, b) => 1 + size(a) + size(b)
    case Leaf(_) => 1
  }

  def maximum(l: Tree[Int]): Int = l match {
    case Branch(a, b) => maximum(a).max(maximum(b))
    case Leaf(a) => a
  }

  def depth[A](l: Tree[A]): Int = l match {
    case Branch(a, b) => 1 + depth(a).max(depth(b))
    case Leaf(_) => 0
  }

  def map[A, B](l: Tree[A])(f: A => B): Tree[B] = l match {
    case Branch(a, b) => Branch(map(a)(f), map(b)(f))
    case Leaf(a) => Leaf(f(a))
  }

  //不明
  def fold[A, B](l: Tree[A])(f: A => B)(g: (B, B) => B): B = l match {
    case Branch(a, b) => g(fold(a)(f)(g), fold(b)(f)(g))
    case Leaf(a) => f(a)
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}
