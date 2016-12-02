import fpinscala.laziness.Stream

val x = Stream(1,2,3,4)

val y = x.drop(2).toList

val z = x.takeWhileViaFoldRight(_%2==0).toList

val w = x.filter(_%2==0).toList