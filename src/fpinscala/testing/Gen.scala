package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import scala.collection.immutable.Seq

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




case class Prop(run: (TestCases,RNG) => Result){
  def &&(p: Prop): Prop = Prop((testCases,rng)=>{
    run(testCases,rng) match {
      case Falsified => Falsified
      case _ => p.run(testCases,rng)
    }
  })

  def ||(p: Prop): Prop = Prop((testCases,rng)=> {run(testCases,rng)match{
    case Passed => Passed
    case _ => p.run(testCases,rng)
  }})
}

object Prop {

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified = false
  }

  case object Falsified extends Result {
    override def isFalsified = true
  }
  type TestCases = Int
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(rng => RNG.nonNegativeInt(rng) match {
    case (n,rng2) => (start + n % (stopExclusive-start), rng2)
  }))

  def unit[A](a:A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(rng => RNG.boolean(rng)))

  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] = Gen(State.sequence((1 to n).map(x => gen.sample).toList))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b=> if(b)g1 else g2)

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(x=>listOfN(x,g))
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

case class SGen[+A](forSize: Int => Gen[A])

//trait SGen[+A] {
//
//}

