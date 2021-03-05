sealed trait Par[A]
//case class Para[A](get: A) extends Par[A]

object Par {

  def map2[A,B,C](a: => Par[A], b: => Par[B])(f: (A,B) => C): Par[C] =
}

println("yes")