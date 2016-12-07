import fpinscala.laziness.Stream

val x = Stream(1,2,3,4)
val y = Stream(1,4)

val z = x.tails.map {_.toList}.toList