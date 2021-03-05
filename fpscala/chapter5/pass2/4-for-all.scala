sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }

  def toList: List[A] = {

    def go(s: Stream[A]): List[A] = s match {
        case Empty => Nil:List[A]
        case Cons(h,t) => h()::go(t())
      }
    go(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b )

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b )
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

println(Stream(1,2,3).exists(x => x == 2))
println(Stream(2,2,3).exists(x => x > 4))

println(Stream(2,2,3).forAll(x => x > 2))
println(Stream(2,2,3).forAll(x => x > 1))

