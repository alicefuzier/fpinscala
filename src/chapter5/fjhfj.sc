def maybeTwice(b: Boolean, i: => Int) = if (b)
  {
    lazy val j = i
    println("Bu")
    j + j
  }
else 0

val x = maybeTwice(true,{println("ciao"); 1+3})

val y = if (true) {{println("ciao"); 1+3}}+{println("ciao"); 1+3} else 0