import scala.collection.immutable.List
import scala.collection.immutable.List._
import scala.collection.immutable.Nil

sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
case class C[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def c[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    C(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else {
      c(as.head, apply(as.tail: _*))
    }
  }

  def drop[A](s: => Stream[A], n: Int): List[A] = {

    def _go(s: => Stream[A], l: List[A], n: Int): List[A] = {

      s match {
        case C(h,t) => if (n > 0 ) _go(t(),l, n - 1 ) else _go(t(),h()::l, n - 1 )
        case Empty => l
      }
    }

    _go(s, Nil, n).reverse
  }
}


println(Stream.drop(Stream(1,2,3),1))
println(Stream.drop(Stream(1,2,3),2))
