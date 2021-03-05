sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  @annotation.tailrec
  def foldLeft[A,B](as:List[A],z:B)(f:(B,A)=>B): B =
    as match {
      case Cons(h,t) => foldLeft(t,f(z,h))(f)
      case Nil => z
    }

  def sum(as:List[Int]): Int = foldLeft(as,0)(_ + _)

  def product(as:List[Int]): Double = foldLeft[Int,Double](as,1)(_ * _)
}

import List._

println(foldLeft(List(1,2,4),1)(_ * _))
println(sum(List(1,2,4)))
println(product(List(1,2,4)))