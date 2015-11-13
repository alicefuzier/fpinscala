def fib(n: Int): Int = {
  def go(a: Int, b: Int, n: Int): Int = {
    if(n == 0) a
    else go(b, a+b, n-1)
  }
  go(0, 1, n)
}

(0 to 10 ) map(fib(_))