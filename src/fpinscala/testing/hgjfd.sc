import fpinscala.testing.{Gen, Prop}

val smallInt = Gen.choose(1,2)

val prop = Prop.forAll(Gen.listOf1(smallInt))((ns) => {
  ns.sorted.head == ns.min
}
)