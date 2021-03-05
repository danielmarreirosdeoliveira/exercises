case class S[A](a: A) {

  def flatMap[B](g: A => S[B]): S[B] = {
    println("flatMap1",this.a)
    val ga = g(this.a)
    println("flatMap2",ga)
    ga
  }


  def map[B](f: A => B): S[B] = {
    println("map",this.a)
    this.flatMap((a: A) => {
      println(":"+a)
      println(":"+f(a))
      S(f(a))
    })
  }
}

val s: S[String] = for {
  x <- S("Hello")
  y <- S(x+", ")
} yield y+"world!"
//
//
// ... equivalent to:
//
//val s = S("Hello")
//  .flatMap(x => S(x+", ")
//    .map(y => y+"world!"))

println(s)