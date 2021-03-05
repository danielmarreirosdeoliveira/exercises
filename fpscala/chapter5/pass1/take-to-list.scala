import scala.collection.immutable.List
import scala.collection.immutable.Nil

sealed trait Stream[+A] {
  def take[A](t: => Stream[A], n: Int): List[A] = {

    def _go(t: => Stream[A], l: List[A], n: Int): List[A] = {
      if (n==0) return l
      t match {
        case C(h,t) => _go(t(),h()::l,n-1)
        case Empty => return l;
      }

    }

    _go(t, Nil, n).reverse
  }

  def toList[A](as:Stream[A]):List[A] = {
    println("h")
    as match {
      case (Empty) => Nil
      case C(h,t) => { println(h()); h()::toList(t())}
    }
  }
}
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
}


println(Stream(1,2,3).take(2).toList)
