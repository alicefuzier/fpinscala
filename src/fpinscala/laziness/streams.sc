import fpinscala.laziness.Stream._

val x = Stream(1,2,3)

val y = x.drop(2).toList

val z = x.takeWhile(_<=2).toList