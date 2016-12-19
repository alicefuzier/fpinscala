package fpinscala.parallelism

class Par[A] {
}

object Par {
  def map2[A,B,C](pa: Par[A],pb: Par[B])(f:(A,B)=>C):Par[C] = ???
}