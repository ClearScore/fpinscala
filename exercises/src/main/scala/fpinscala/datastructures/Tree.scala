package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25
  def size[A]: Tree[A] => Int = {
    case _: Leaf[_]          => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  // Exercise 3.26
  def maximum: Tree[Int] => Int = {
    case Leaf(l)             => l
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  // Exercise 3.27
  def depth[A]: Tree[A] => Int = {
    case _: Leaf[_]          => 1
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  // Exercise 3.28
  def map[A, B](f: A => B): Tree[A] => Tree[B] = {
    case Leaf(l)             => Leaf(f(l))
    case Branch(left, right) => Branch(map(f)(left), map(f)(right))
  }

  // Exercise 3.29
  def fold[A, B](f: A => B)(g: (B, B) => B): Tree[A] => B = {
    def rec: Tree[A] => B = fold(f)(g)

    {
      case Leaf(l)             => f(l)
      case Branch(left, right) => g(rec(left), rec(right))
    }
  }

  def size2[A]: Tree[A] => Int = fold[A, Int](_ => 1)(_ + _ + 1)
  def maximum2: Tree[Int] => Int = fold[Int, Int](i => i)((l, r) => l max r)
  def depth2[A]: Tree[A] => Int = fold[A, Int](_ => 1)((l, r) => (l max r) + 1)
  def map2[A, B](f: A => B): Tree[A] => Tree[B] = fold[A, Tree[B]](v => Leaf(f(v)))(Branch(_, _))
}
