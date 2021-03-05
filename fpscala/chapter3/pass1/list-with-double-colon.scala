sealed trait List[+A] {
  def foldLeft[A,B](as:List[A],z:B)(f:(B,A)=>B): B =
    as match {
      case ::(h,t) => foldLeft(t,f(z,h))(f)
      case Nil => z
    }
  def reverseViaFoldLeft(): List[A] =
    foldLeft(this, Nil:List[A])((z,h) => ::(h,z))
}
case object Nil extends List[Nothing]
case class ::[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else ::(as.head, apply(as.tail: _*))

}

println((0::List(1,2)).reverseViaFoldLeft())















