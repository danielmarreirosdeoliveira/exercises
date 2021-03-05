sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }

  def toList: List[A] = {

    def go(s: Stream[A]): List[A] = {
      s match {
        case Empty => Nil:List[A]
        case Cons(h,t) => h()::go(t())
      }
    }

    go(this)
  }

  def take(n: Int): Stream[A] = {

    def go(s: Stream[A], i: Int): Stream[A] = {
      s match {
        case Empty => Empty
        case Cons(h, t) => if (i < n) Cons(h, () => go(t(), i + 1)) else Empty
      }
    }
    go(this, 0)
  }

  def drop(n: Int): Stream[A] = {

    def go(s: Stream[A], i: Int): Stream[A] = {
      s match {
        case Empty => Empty
        case Cons(h, t) => if (i < n) go(t(), i + 1) else Cons(h, () => go(t(), i + 1))
      }
    }
    go(this, 0)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {

    def go(s: Stream[A]): Stream[A] = {
      s match {
        case Empty => Empty
        case Cons(h, t) => if (p(h())) Cons(h, () => go(t())) else Empty
      }
    }
    go(this)
  }
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


println(Stream("1","2","3").takeWhile(a => a != "2").toList)
println(Stream("1","2","3").takeWhile(a => a != "3").toList)
println(Stream("1","2","3").takeWhile(a => a != "4").toList)

