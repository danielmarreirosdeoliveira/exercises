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



def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(mean =>
    Some(xs.foldRight(0.0)((x,b) => b + math.pow(x - mean, 2)) / xs.length)
  )


println(variance(List(1,1,1,9,9,9)))
println(variance(List(4,4,4,6,6,6)))
println(variance(List()))