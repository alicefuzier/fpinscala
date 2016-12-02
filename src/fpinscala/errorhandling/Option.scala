package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match{
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
  def orElse[B>:A](ob: => Option[B]): Option[B] = map(x=>Some(x)) getOrElse ob
  def orElse_2[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(x) => Some(x)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match{
    case Some(x) if f(x) => Some(x)
    case _ => None
  }

  def filter2(f: A => Boolean): Option[A] = flatMap(x=>if(f(x)) Some(x) else None)


}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = mean(xs)flatMap{
    m=> mean(xs.map(x=>math.pow(x - m, 2)))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap ((aa: A) => {
      b map (bb => f(aa, bb))
    }: Option[C])
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight(Some(List[A]()):Option[List[A]])((aa,res)=> map2(aa,res)((aaa,ress)=> aaa :: ress) )
  def sequence2[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: tail => h flatMap (head => sequence2(tail) map (l=> head :: l))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a.map(f(_)))
  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight(Some(Nil): Option[List[B]])((aa,acc)=> map2(f(aa),acc)(_ :: _))
  def traverse3[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: tail => map2(f(h), traverse3(tail)(f))(_::_)
  }
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x=>x)
}