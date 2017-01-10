package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.testing.Prop.{Falsified, _}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

//
//trait Prop {
//  def check: Boolean
//  def &&(p: Prop): Prop = new Prop {
//    def check: Boolean = this.check && p.check
//  }
//}




case class Prop(run: (MaxSize,TestCases,RNG) => Result){
  def &&(p: Prop) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: Prop) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(max,n,rng)
      case x => x
    }
  }

  def tag(msg: String) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {

  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(rng => RNG.nonNegativeInt(rng) match {
    case (n,rng2) => (start + n % (stopExclusive-start), rng2)
  }))

  def unit[A](a:A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(rng => RNG.boolean(rng)))

  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] = Gen(State.sequence((0 to n).map(x => gen.sample).toList))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b=> if(b)g1 else g2)

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(x=>listOfN(x,g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(x=>listOfN(x max 1,g))
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(x=>f(x).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(x=> Gen.listOfN(x,this))

  def unsized: SGen[A] = SGen(n=>this)
}

case class SGen[+A](forSize: Int => Gen[A]){
  def apply(n: Int): Gen[A] = forSize(n)
}

//trait SGen[+A] {
//
//}

