sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def head[A](as: List[A], a: A): List[A] = as match {
    case Nil => as
    case Cons(h,t) => Cons(a,t)
  }
}

println(List.head(List(),2))
println(List.head(List(1),2))
println(List.head(List(1,2,3),2))
