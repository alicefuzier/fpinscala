package fpinscala.state

import fpinscala.state.State.{get, sequence, unit}

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i,rng2) = nonNegativeInt(rng)
    (i/(Int.MaxValue.toDouble+1), rng2)
  }

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(i=> i/(Int.MaxValue.toDouble+1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i,rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i,d),rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d),r)=intDouble(rng)
    ((d,i),r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    ((d1, d2, d3),r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count == 0)
      (List(),rng)
    else {
      val (i,r1) = rng.nextInt
      val (l,r2) = ints(count-1)(r1)
      (i::l,r2)
  }}

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b,r2) = rb(r1)
    (f(a,b), r2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldRight((List[A](),rng))((r,tuple)=> tuple match {
      case (list, currentRng) =>
        val (element,newRng) = r(currentRng)
        (element :: list, newRng)
    })
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((randa,randacc) => map2(randa,randacc)(_::_))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a,r1) = f(rng)
    g(a)(r1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i =>{
    val mod = i%n
    if(i+(n-1)-mod >=0) unit(mod) else nonNegativeLessThan(n)
  })

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(x=>unit(f(x)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b=>f(a,b)))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(x=>unit(f(x)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b=>f(a,b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(rng =>{
    val (a, r1) = this.run(rng)
    f(a).run(r1)
  })
}



object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] =
    State(rng => (a, rng))

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit[S,List[A]](List[A]()))((statea,stateacc) => statea.map2(stateacc)(_::_))


  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def modify2[S](f: S => S): State[S, Unit] = get.flatMap(x=> set(f(x)))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int){
  def coinInserted: Machine = if(candies >0 && locked) Machine(locked=false,candies = candies, coins = coins +1) else this
  def knobTurned: Machine = if (!locked) Machine(locked = true, candies = candies -1, coins = coins) else this
}
object CandyMachine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence[Machine, Unit](inputs.map {
      case Coin => State[Machine, Unit](machine => ((), machine.coinInserted))
      case Turn => State[Machine, Unit](machine => ((), machine.knobTurned))
    })
    s <- get
  } yield (s.coins, s.candies)
}