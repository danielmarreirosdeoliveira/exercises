sealed trait List[+A]
case class Nil() extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil()
    else Cons(as.head, apply(as.tail: _*))

  def sum(as: List[Int]): Int = as match {
      case Nil() => 0
      case Cons(h,t) => h + sum(t)
    }


  def product(as: List[Int]): Double = as match {
      case Nil() => 1.0
      case Cons(0.0,_) => 0.0
      case Cons(h,t) => h * product(t)
    }

}

println(List.sum(List(1,2,3)))
println(List.product(List(1,2,3)))
println(List(1,2,"a"))