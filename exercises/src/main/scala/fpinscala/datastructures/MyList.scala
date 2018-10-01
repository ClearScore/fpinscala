package fpinscala.datastructures

sealed trait MyList[+A] // `List` data type, parameterized on a type, `A`
case object MyNil extends MyList[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: MyList[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case MyNil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: MyList[Double]): Double = ds match {
    case MyNil        => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] = // Variadic function syntax
    if (as.isEmpty) MyNil
    else Cons(as.head, apply(as.tail: _*))

  val res: Int = MyList(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case MyNil                                 => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    a1 match {
      case MyNil      => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case MyNil       => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: MyList[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: MyList[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: MyList[A]): MyList[A] = l match {
    case Cons(_, tail) => tail
    case MyNil         => MyNil
  }

  def setHead[A](l: MyList[A], h: A): MyList[A] = l match {
    case Cons(_, tail) => Cons(h, tail)
    case MyNil         => Cons(h, MyNil)
  }

  def drop[A](l: MyList[A], n: Int): MyList[A] = l match {
    case Cons(_, tail) if n > 0 => drop(tail, n - 1)
    case Cons(_, tail)          => tail
    case MyNil                  => MyNil
  }

  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = l match {
    case Cons(h, tail) if f(h) => dropWhile(tail, f)
    case _                     => l
  }

  def init[A](l: MyList[A]): MyList[A] = {
    l match {
      case Cons(_, MyNil) => MyNil
      case Cons(h, tail)  => Cons(h, init(tail))
      case MyNil          => MyNil
    }
  }

  def init2[A](l: MyList[A]): MyList[A] = {
    def loop(acc: MyList[A], eval: MyList[A]): MyList[A] = eval match {
      case Cons(_, MyNil) => acc
      case Cons(h, tail)  => loop(Cons(h, acc), tail)
      case MyNil          => acc
    }

    reverse(loop(MyNil, l))
  }

  def length[A](l: MyList[A]): Int = {
    def loop(acc: Int, list: MyList[A]): Int = list match {
      case Cons(_, tail) => loop(acc + 1, tail)
      case MyNil         => acc
    }

    loop(0, l)
  }

  def reverse[A](l: MyList[A]): MyList[A] = {
    def loop(list: MyList[A], acc: MyList[A]): MyList[A] = list match {
      case Cons(head, tail) => loop(tail, Cons(head, acc))
      case MyNil            => acc
    }

    loop(l, MyNil)
  }

  def foldLeft[A, B](l: MyList[A], z: B)(f: (A, B) => B): B = l match {
    case Cons(head, tail) => foldLeft(tail, f(head, z))(f)
    case MyNil            => z
  }

  def sum3(l: MyList[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: MyList[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: MyList[A]): Int = foldLeft(l, 0) {
    case (_, acc) => acc + 1
  }

  def foldLeftFromFoldRight[A, B](as: MyList[A], base: B)(f: (A, B) => B): B =
    foldRight(as, (b: B) => b)((a, g) => g compose (b => f(a, b)))(base)

  def foldRightFromFoldLeft[A, B](as: MyList[A], base: B)(f: (A, B) => B): B = {
    val func = foldLeft(as, (b: B) => b)((a, g) => g compose (b => f(a, b)))
    func(base)
  }

  // Exercise 3.14
  def append[A, U >: A]: (MyList[A], U) => MyList[U] =
    (as, u) => foldRight(as, MyList(u))(Cons(_, _))

  // Exercise 3.15
  def flatten[A](as: MyList[MyList[A]]): MyList[A] =
    foldRight(as, MyNil: MyList[A])((bs, ns) => foldRight(bs, ns)(Cons(_, _)))

  def increment(as: MyList[Int]): MyList[Int] =
    foldRightFromFoldLeft(as, MyNil: MyList[Int]) {
      case (i, acc) => Cons(i + 1, acc)
    }

  def conv(as: MyList[Double]): MyList[String] =
    foldRightFromFoldLeft(as, MyNil: MyList[String]) {
      case (i, acc) => Cons(i.toString, acc)
    }

  def map[A, B](as: MyList[A])(f: A => B): MyList[B] =
    foldRightFromFoldLeft[A, MyList[B]](as, MyNil) {
      case (a, acc) => Cons(f(a), acc)
    }

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    foldRightFromFoldLeft[A, MyList[A]](as, MyNil) {
      case (a, acc) if f(a) => Cons(a, acc)
      case (_, acc)         => acc
    }

  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] =
    foldRightFromFoldLeft(as, MyNil: MyList[B])(
      (a, res) => foldRightFromFoldLeft(f(a), res)(Cons(_, _))
    )

  def flatMapFilter[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(as) {
      case a if f(a) => Cons(a, MyNil)
      case _         => MyNil
    }

  def lSum(as: MyList[Int], bs: MyList[Int]): MyList[Int] =
    (as, bs) match {
      case (Cons(a, aTail), Cons(b, bTail)) => Cons(a + b, lSum(aTail, bTail))
      case _                                => MyNil
    }

  def zipWith[A, B, C](l1: MyList[A], l2: MyList[B])(f: (A, B) => C): MyList[C] = (l1, l2) match {
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
    case _                          => MyNil
  }

  // Exercise 3.24
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = {
    def isSubsequence: (MyList[A], MyList[A]) => Boolean = {
      case (_, MyNil) => true
      case (Cons(a, as), Cons(b, bs)) if a == b => isSubsequence(as, bs)
      case _ => false
    }

    (sup, sub) match {
      case (_, MyNil) => true
      case (MyNil, _) => false
      case (Cons(a, as), Cons(b, bs)) if a == b && isSubsequence(as, bs) => true
      case (Cons(_, as), bs) => hasSubsequence(as, bs)
    }
  }

}
