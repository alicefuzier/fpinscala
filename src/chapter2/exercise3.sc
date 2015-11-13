def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
  (a: A) => f(a, _)
}