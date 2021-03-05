sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  @annotation.tailrec
  def foldLeft[A,B](as:List[A],z:B)(f:(B,A)=>B): B = as match {
    case Cons(h,t) => foldLeft(t,f(z,h))(f)

    case Nil => z
  }

  def reverseViaFoldLeft[A](as:List[A]): List[A] =
    foldLeft(as, Nil:List[A])((z,h) => Cons(h,z))

  def add_one(as: List[Int]): List[Int] =
    reverseViaFoldLeft(foldLeft[Int,List[Int]](as:List[Int],Nil:List[Nothing])((x: List[Int],y: Int)=>
      Cons(y+1,x)
    ))
}

import List._

println(add_one(List(1,2,5)))
