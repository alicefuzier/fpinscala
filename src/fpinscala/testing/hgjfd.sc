import fpinscala.testing.{Gen, Prop}

val smallInt = Gen.choose(1,2)

val prop = Prop.forAll(smallInt)((ns)=>
  val max = ns.max
  !ns.exists(_ > max)
)