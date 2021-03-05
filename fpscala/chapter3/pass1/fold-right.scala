sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as:List[A],z:B)(f:(A,B)=>B): B =
    as match {
      case Nil => z
      case Cons(x,xs) => f(x, foldRight(xs, z)(f))
    }

  def smallerThan(as:List[Int],b:Int): Boolean =
    foldRight(as,true)((x,y) => (x < b) && y)
}

println(List.foldRight[Int,Int](List(1,2,4),1)(_ * _))
println(List.foldRight[Int,Boolean](List(1,2,4),true)( (x,y) => (x < 5) && y))
println(List.smallerThan(List(1,2,4),5))
println(List.foldRight[Int,Boolean](List(1,2,4),true)(_ < 5 && _))
println(List.foldRight[Int,Boolean](List(1,2,4),true)( (x,y) => (x < 2) && y))
println(List.smallerThan(List(1,2,4),2))
println(List.foldRight[Int,Boolean](List(1,2,4),true)(_ < 2 && _))
