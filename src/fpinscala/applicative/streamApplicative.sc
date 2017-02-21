import fpinscala.applicative.Applicative


val stream1 = Stream(1,2,3)
val stream2 = Stream(4,5)
val stream3 = Stream(6,7,8,9)


val res = Applicative.streamApplicative.sequence(List(stream1,stream2,stream3)).toList
