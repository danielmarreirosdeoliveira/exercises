

println(List(1,2,3))
println(1::2::Nil)

val abc = List("A", "B", "C")

def add(res: String, x: String) = {
  println(s"op: $res + $x = ${res + x}")
  res + x
}

// println(abc.reduceLeft(add))
// println(abc.reduceRight(add))

// println(abc.foldLeft("z")(add))
println(abc.scanLeft("z")(add))