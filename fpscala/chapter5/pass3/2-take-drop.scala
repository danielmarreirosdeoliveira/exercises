import Stream._

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h()::t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1) )
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] =
    if (n == 0) this
    else this match {
      case Cons(_, t) => t().drop(n - 1)
      case _ => empty
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons( () => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}


println(Stream("1", "2", "3").take(0).toList)
println(Stream("1", "2", "3").take(1).toList)
println(Stream("1", "2", "3").take(2).toList)
println(Stream("1", "2", "3").take(3).toList)
println(Stream("1", "2", "3").take(4).toList)

println(Stream("1", "2", "3").drop(0).toList)
println(Stream("1", "2", "3").drop(1).toList)
println(Stream("1", "2", "3").drop(2).toList)
println(Stream("1", "2", "3").drop(3).toList)
println(Stream("1", "2", "3").drop(4).toList)


