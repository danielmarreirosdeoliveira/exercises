sealed trait Option[+A] {

  def getOrElse[B >: A](default: => B): B = this match { // pm
    case None => default
    case Some(x) => x
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(a => Some(a)).getOrElse(ob)

  def map[B](f: A => B): Option[B] = this match { // pm
    case None => None
    case Some(x) => Some(f(x))
  }
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  a.flatMap[C](iA => b.map[C](iB => f(iA, iB)))
}

def sequence[A](a: List[Option[A]]): Option[List[A]] =
  a.foldRight(Some(Nil):Option[List[A]])((oa,ola) =>

    map2(oa, ola)((a, la) => a::la)

  ).flatMap(lista => if (lista.isEmpty) None else Some(lista))


println(sequence(List(Some(1),Some(2),Some(3))))
println(sequence(List(Some(1),None,Some(3))))
println(sequence(List()))