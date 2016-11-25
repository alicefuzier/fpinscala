import fpinscala.datastructures.List
import fpinscala.datastructures.Cons
import fpinscala.datastructures.Nil
List.filterViaFlatMap(List(1,2,3))(i=> i>=2)