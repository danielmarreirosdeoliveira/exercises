sealed trait Str[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Con(h,t) => Some(h())
  }
}
case object Empty extends Str[Nothing]
case class Con[+A](h: () => A, t: () => Str[A]) extends Str[A]

object Str {
  def con[B](hd: => B, t1: => Str[B]): Str[B] = {
    lazy val head = hd
    lazy val tail = t1
    Con(() => head, () => tail)
  }
}

val one = Con[String](() => {println("!"); "a"}, () => Con( () => "b", () => Empty: Str[String]))
println(one)
println(one.headOption)
println(one.headOption)



val two = Str.con({println("hi"); "a"}, Str.con( "b", {Empty:Str[String]}))
println(two)
println(two.headOption)
println(two.headOption)