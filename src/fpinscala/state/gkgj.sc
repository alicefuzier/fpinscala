import fpinscala.state._

val machine: State[Machine, (Int, Int)] = CandyMachine.simulateMachine(List(Coin,Turn, Turn, Coin,Coin,Turn,Coin))

val tuple = machine.run(Machine(true,2,0))
tuple._1
tuple._2
