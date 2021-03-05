import scala.collection.immutable.List
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
    println("call apply")
    if (as.isEmpty) empty
    else {
      c(as.head, apply(as.tail: _*))
    }
  }
  
  def toList[A](as:Stream[A]):List[A] = {
    as match {
      case (Empty) => Nil
      case C(h,t) => h()::toList(t())
    }
  }
}

println(Stream.toList(Stream(1,2,3)))
