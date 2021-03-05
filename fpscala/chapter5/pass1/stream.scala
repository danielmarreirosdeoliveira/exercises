sealed trait Stream[+A]
case object Nil extends Stream[Nothing]
case class C[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def c[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    C(() => head, () => tail)
  }

  def nil[A]: Stream[A] = Nil

  def apply[A](as: A*): Stream[A] = {
    println("call apply")
    if (as.isEmpty) nil
    else {
      c(as.head, apply(as.tail: _*))
    }
  }

  def first[A](as: Stream[A]) = as match {
    case Nil => Nil
    case C(h,_) => h()
  }

  def rest[A](as: Stream[A]) = as match {
    case Nil => Nil
    case C(_,t) => t()
  }
}

//println(Stream(1,2,3))
println(Stream.first(Stream.rest(Stream(1,2,3))))
//println(Stream.first(Stream(1,2,3)))