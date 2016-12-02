package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

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

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(_, a) => a
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(a, rest) => Cons(h, rest)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(a, b) => drop(b, n - 1)
      }
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => l
      case Cons(a, b) => if (f(a)) dropWhile(b, f) else l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => l
      case Cons(a, Cons(b, Nil)) => Cons(a, Nil)
      case Cons(a, b) => init(b)
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((a: A, b: Int) => b + 1)
  }

  @annotation.tailrec
  def foldLeft[A1, B1](l: List[A1], z: B1)(f: (B1, A1) => B1): B1 = {
    l match {
      case Nil => z
      case Cons(a, b) => foldLeft(b, f(z, a))(f)
    }
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((b, _) => b + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((b: List[A], a: A) => Cons(a, b))
  }

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g: (B) => B, a: A) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRightViaFoldLeft(a1, a2)((a: A, b: List[A]) => Cons(a, b))

  def concat[A](lists: List[List[A]]): List[A] = foldRightViaFoldLeft(lists, Nil: List[A])((a: List[A], b: List[A]) => append2(a, b))

  def add1(integers: List[Int]): List[Int] = foldRight(integers, Nil: List[Int])((a: Int, b: List[Int]) => Cons(a + 1, b))

  def doubleTotString(doubles: List[Double]): List[String] =
    foldRight(doubles, Nil: List[String])((a: Double, b: List[String]) => Cons(a.toString, b))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((h, t) => append2(f(h),t))

  def flatMap2[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a=>if(f(a)) List(a) else Nil)

  def addElementsOf(a1: List[Int], a2: List[Int]): List[Int] = (a1,a2) match {
    case (Nil,_ )=> Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addElementsOf(t1,t2))
  }

  def zipWith[A,B,C](a1: List[A], a2: List[B])(f:(A,B)=>C): List[C] = (a1,a2) match {
    case (Nil,_ )=> Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sub, sup)match{
      case(_,Nil)=>true
      case (Cons(h1,t1),Cons(h2,t2)) if h1 == h2 => startsWith(t1,t2)
      case _ => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) =>true
    case Cons(h,t) => hasSubsequence(t,sub)
  }
}