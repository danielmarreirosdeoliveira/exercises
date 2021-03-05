import Stream._

sealed trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h()::t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((a, b) => cons(f(a), b))

  def append[B >: A](z: => Stream[B]): Stream[B] =
    foldRight(z)((a, b) => cons(a, b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((h, t) => if (f(h)) cons(h, t) else t)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => f(h) append t)
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


println(Stream(1, 2, 3).map(_ * 2).toList)
println(Stream(1, 2, 3).append(Stream(4, 5)).toList)
println(Stream(1, 2, 3).filter(_ > 1).toList)
println(Stream(1, 2, 3).flatMap(i => Stream(i, i * 2, i * 3)).toList)
// println(empty.headOption)


