sealed trait List[+A] {
  def tl(): List[A] = {
    this match {
      case Nil => Nil
      case Cons(h,t) => t
    }
  }
}
case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))



  def drop[A](as: List[A], n: Int): List[A] = {

    def _go(as: List[A], n: Int): List[A]= {
      if (n == 0) return as

      _go(as.tl(), n-1)
    }

    _go(as, n)
  }
}

println(List.drop(List(1,2,3),2))
