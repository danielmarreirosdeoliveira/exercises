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

  def flatMap2[B](f: A => Option[B]): Option[B] = this match { // extra
    case None => None
    case Some(x) => f(x)
  }
  def mapViaFlatMap[B](f: A => B): Option[B] = // extra
    flatMap(x => Some(f(x))).orElse(None)

  def mapViaFlatMap2[B](f: A => B): Option[B] = // extra
    flatMap2(x => Some(f(x))).orElse(None)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

val abs0: Option[Double] => Option[Double] = lift(math.abs)

println(abs0(Some(2.0)))
println(abs0(Some(-2.0)))
println(abs0(None))