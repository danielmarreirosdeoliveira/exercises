def compose[A,B,C](f: B => C, g: A => B): A => C =
  a => f(g(a))


val asString = (x: Int) => x.toString
val equals1 = (x: String) => if (x == "1") true else false


val asStringEquals1 = compose(equals1, asString)
val asStringEquals1_v2 = (x: Int) => if (x.toString == "1") true else false


println(asStringEquals1(1))
println(asStringEquals1(2))
println(asStringEquals1_v2(1))
println(asStringEquals1_v2(2))
