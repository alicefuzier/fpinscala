package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toList
  }

  def toListTailRecursive: List[A] = {
    def go(stream: Stream[A], acc: List[A]): List[A] =stream match {
      case Empty => acc
      case Cons(h,t) => go(t(),h() :: acc)
    }
    go(this, List()).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match{
    case Empty => Empty
    case Cons(h,t) if n > 1 => cons(h(),t().take(n-1))
    case Cons(h,_) if n==1 => cons(h(),Stream.empty)
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(_,t) if n>1 => t().drop(n-1)
    case Cons(_,t) if n==1 => t()
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(),t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a,b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = this.foldRight[Stream[A]](empty)((a, b) => if(p(a)) cons(a, b) else empty)

  def headOption: Option[A] = this.foldRight[Option[A]](None)((a,b)=>Some(a))

  def map[B](f:A=>B): Stream[B]= this.foldRight[Stream[B]](empty)((a,b)=>cons(f(a),b))

  def filter(f:A=>Boolean): Stream[A]= this.foldRight[Stream[A]](empty)((a,b)=> if(f(a)) cons(a,b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] = this.foldRight(s)((a,b)=> cons(a,b))

  def flatMap[B](f:A=>Stream[B]): Stream[B] = this.foldRight[Stream[B]](empty)((a,b)=>f(a)append b)

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = cons(n,from(n+1))

  def fibs: Stream[Int]={
    def go(a:Int,b: Int): Stream[Int]={
      cons(a+b,go(b,a+b))
    }
    cons(0,cons(1,go(0,1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")

  def constant[A](a:A): Stream[A] = cons(a,constant(a))
}