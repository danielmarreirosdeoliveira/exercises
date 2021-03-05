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


  @annotation.tailrec
  def foldLeft[A,B](as:List[A],z:B)(f:(B,A)=>B): B =
    as match {
      case Cons(h,t) => foldLeft(t,f(z,h))(f)
      case Nil => z
    }

    def foldLeftViaFoldRight[A,B](as:List[A],z:B)(f:(B,A)=>B): B =

      foldRight(as, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
}

import List._

println(foldLeft(List(1,2,3),0)(_ + _))