def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a,b) => f(a)(b)

val fun1 = (a: Int) => (b: Double) => if ((a == 1) && (b == 2.0)) true else false
val fun2 = uncurry(fun1)

println(fun2(1, 2.0))
println(fun2(0, 2.0))
println(fun2(1, 2.3))