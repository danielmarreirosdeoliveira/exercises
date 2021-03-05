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

  def reverse[A](as:List[A]): List[A] =
    foldLeft(as, Nil:List[A])((z,h) => Cons(h,z))

//  def tails[A](as: List[A]): List[List[A]] =
//    foldRight(as, (Nil:List[A],Nil:List[List[A]]))((a,b)=>(Cons(a,b._1),Cons(Cons(a,b._1),b._2)))._2

//  def scanRight[A,B](as:List[A], z: B)(f: (A,B) => B): List[B] =
//    foldRight(tails(as),List(z))((a,b) => Cons( foldRight(a, z)(f) , b))

  def scanRight[A,B](as: List[A], z: B)(f: (A,B) => B): List[B] =
    foldRight(as, (z, List(z)))((a,b)=> (f(a,b._1),  Cons(f(a,b._1),b._2)))._2

  def scanLeft[A,B](as: List[A], z: B)(f: (A,B) => B): List[B] =
    reverse(foldLeft(as, (z, List(z)))((b,a)=> (f(a,b._1),  Cons(f(a,b._1),b._2)))._2)

  def tails[A](as: List[A]): List[List[A]] =
    foldRight(as, (Nil:List[A], Nil:List[List[A]]))((a,b)=> (Cons(a,b._1),  Cons(Cons(a,b._1),b._2)))._2
//    scanRight(as, Nil:List[A]){(a,b) => println("b",b); Cons(a,b)}

  def starts[A](as: List[A]): List[List[A]] =
    reverse(foldLeft(as, (Nil:List[A], Nil:List[List[A]]))((b,a)=> (Cons(a,b._1),  Cons(reverse(Cons(a,b._1)),b._2)))._2)
}

//println(List.scanRight(List(1,2,3), 0)(_ + _))
println(List.scanLeft(List(1,2,3), 0)(_ + _))
println(List.tails(List(1,2,3)))
println(List.starts(List(1,2,3)))



// Nil
// 1 Nil