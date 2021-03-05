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

  def appendViaFoldLeft[A](as:List[A],bs:List[A]): List[A] =
    foldLeft(List.reverseViaFoldLeft(as),bs)((akk,h) => Cons(h,akk))

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, Nil:List[B])((z,h)=>appendViaFoldLeft(z,f(h)))
}

import List._

println(flatMap(List(1,2,3))(i => List(i, i)))
