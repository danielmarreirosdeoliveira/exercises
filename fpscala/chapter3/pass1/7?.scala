sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as:List[A],z:B,f:(A,B)=>B): ()=>B =
    as match {
      case Nil => ()=>z
      case Cons(x,xs) => {
        println(x)
        f(x, foldRight(xs, z,f))
      }
    }

}

println(List.foldRight[Int,Int](List(1,0,4),1,
  (x,y: (xs:List[Int],z:Int,f:(Int,(Int)=>Int)=>Int)=>Int)

  => {lazy val z = y; if (x==0) ()=>(0:Int) else x * z()}))


