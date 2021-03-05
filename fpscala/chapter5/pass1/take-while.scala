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

  def takeWhile[A](s: => Stream[A], p: (A) => Boolean ): List[A] = {

    def _go(s: => Stream[A], l: List[A], p: (A) => Boolean ): List[A] = {

      s match {
        case C(h,t) => if (p(h())) _go(t(),h()::l,p) else l
        case Empty => Nil
      }
    }

    _go(s, Nil, p).reverse
  }
}


println(Stream.takeWhile[Int](Stream[Int](1,2,3),(n: Int) => n < 3))
println(Stream.takeWhile(Stream(1,2,3),(n: Int) => n < 1))
println(Stream.takeWhile[Int](Stream(1,2,3),n => n < 1))
println(Stream.takeWhile[Int](Stream(1,2,3),_ < 2))
