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

  def zipWith[A](as1: List[A], as2: List[A])(f: (A,A) => A): List[A] = {

    @annotation.tailrec
    def go(as1: List[A], as2: List[A], akk: List[A]): List[A] = {

      as1 match {
        case Cons(h,t) => as2 match {
          case Cons(h2,t2) => go(t,t2,Cons(f(h,h2), akk))
          case Nil => akk
        }
        case Nil => akk
      }
    }

    List.reverseViaFoldLeft(go(as1,as2,Nil:List[A]))
  }
}

import List._

println(zipWith(List(1,2,4,5),List(3,2,5,4))(_ + _))
println(zipWith(List(1,2,4,5),List(3,2,5))(_ + _))
println(zipWith(List(1,2),List(3,2,5))(_ + _))
println(zipWith(List("a","b"),List("c","d"))(_ + ":" + _))














