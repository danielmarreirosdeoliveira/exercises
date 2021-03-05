sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil // or as
    case Cons(h,t) => t
  }
}

println(List.tail(List(1,2,3)))
println(List.tail(List(1)))
println(List.tail(List()))
