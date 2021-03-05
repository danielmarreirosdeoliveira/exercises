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



  def dropWhile[A](as: List[A])(p: A => Boolean): List[A] = {

      as match {
        case Cons(h,t) if p(h) => dropWhile(t)(p)
        case _ => as
      }
  }
}

println(List.dropWhile[Int](List(1,2,3))(_ < 4))
println(List.dropWhile(List[Int](1,2,3))(_ < 4))
println(List.dropWhile[Int](List(1,2,3))(_ < 2))
println(List.dropWhile(List(1,2,3))(_ < 2))
println(List.dropWhile[Int](List())(_ < 2))
