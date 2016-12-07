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

  def mapViaUnfold[B](f:A=>B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this,n)) {
    case (Cons(h, t),i) if i>0  => Some((h(), (t(),i-1)))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h, t) if p(h())  => Some((h(), t()))
    case _ => None
  }

  def zipWithViaUnfold[B,C](a2: Stream[B])(f:(A,B)=>C): Stream[C] = unfold((this,a2)){
    case(Cons(h1,t1),Cons(h2,t2)) => Some((f(h1(),h2()),(t1(),t2())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this,s2)){
    case (Empty,Empty) => None
    case (Empty,Cons(h,t)) => Some(((None,Some(h())),(Empty,t())))
    case (Cons(h,t),Empty) => Some(((Some(h()),None),(t(),Empty)))
    case (Cons(h1,t1),Cons(h2,t2)) => Some(((Some(h1()),Some(h2())),(t1(),t2())))
  }

  def startsWith1[B](s: Stream[B]): Boolean = (this,s) match {
    case (_,Empty) => true
    case (Cons(h1,t1),Cons(h2,t2)) if h1()==h2() => t1().startsWith1(t2())
    case _ => false
  }

  def startsWith[B](s: Stream[B]): Boolean = this.zipAll(s).takeWhile(_._2.isDefined).forAll( a=> a._1 == a._2)
  def tails: Stream[Stream[A]] = unfold(this){
    case Empty => None
    case Cons(h,t) => Some(cons(h(),t()),t())
  } append Stream(Empty)

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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a,unfold(s)(f))
    case _ => Empty
  }

  val onesViaUnfold: Stream[Int] = unfold(1)(s => Some((1,s)))
  def constant[A](a:A): Stream[A] = cons(a,constant(a))

  def constantViaUnfold[A](a:A): Stream[A] = unfold(a)(s => Some((a,s)))

  def fromViaUnfold(n:Int): Stream[Int] = unfold(n)(s => Some((s,s+1)))

  def fibsViaUnfold: Stream[Int] = unfold((0,1)){case (a,b) => Some((a+b, (b,a+b)))}
}