import fpinscala.state._

val machine: State[Machine, (Int, Int)] = CandyMachine.simulateMachine(List(Coin,Turn, Turn))

val tuple = machine.run(Machine(true,10,0))
tuple._1
tuple._2
