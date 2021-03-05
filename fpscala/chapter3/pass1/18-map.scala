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

  def reverseViaFoldLeft[A](as:List[A]): List[A] =
    foldLeft(as, Nil:List[A])((z,h) => Cons(h,z))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    reverseViaFoldLeft(foldLeft(as,Nil:List[B])((b,a)=>
      Cons(f(a),b)
    ))
}

import List._

println(map(List(1,2))(_ * 2))
println(map(List(1.0,3.4))(_.toString()+":"))
