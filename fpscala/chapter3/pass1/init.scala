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



  def init[A](as: List[A]): List[A] = {

      as match {
        case Nil => Nil
        case Cons(h,Nil) => Nil
        case Cons(h,t) => Cons(h,init(t))
      }
  }
}

println(List.init[Int](List(1,2,3)))
println(List.init[Int](List(1)))
println(List.init[Int](List()))
