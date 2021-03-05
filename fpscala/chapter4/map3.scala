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
  for {
    iA <- a
    iB <- b
  } yield f(iA,iB)
}

def map3[A,B,C,D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] = {
  for {
    iA <- a
    iB <- b
    iC <- c
  } yield f(iA,iB,iC)
}

println(map3(Some(2), Some(3.0), Some(false))((a,b,c)=> (a==2) && (b==3.0) && (!c)))
println(map3(Some(2), Some(3.0), Some(true))((a,b,c)=> (a==2) && (b==3.0) && (!c)))
println(map3(Some(2), Some(3.0), None)((a,b,c:Boolean)=> (a==2) && (b==3.0) && (!c)))