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


println((None:Option[Int]) map (x => if (x > 3) true else false))
println(Some(3) map (x => if (x > 3) true else false))
println(Some(4) map (x => if (x > 3) true else false))

println((None:Option[Int]) mapViaFlatMap (x => if (x > 3) true else false))
println(Some(3) mapViaFlatMap (x => if (x > 3) true else false))
println(Some(4) mapViaFlatMap (x => if (x > 3) true else false))

println(None orElse Some(3))
println(Some(4) orElse Some(3))

println((None:Option[Int]) map (x => if (x > 3) true else false))
println(Some(3) map (x => if (x > 3) true else false))
println(Some(4) map (x => if (x > 3) true else false))

println((None:Option[Int]) mapViaFlatMap (x => if (x > 3) true else false))
println(Some(3) mapViaFlatMap (x => if (x > 3) true else false))
println(Some(4) mapViaFlatMap (x => if (x > 3) true else false))

println((None:Option[Int]) mapViaFlatMap2 (x => if (x > 3) true else false))
println(Some(3) mapViaFlatMap2 (x => if (x > 3) true else false))
println(Some(4) mapViaFlatMap2 (x => if (x > 3) true else false))

println(Some(4) flatMap (x => if (x > 3) Some(true) else None))
println(Some(3) flatMap (x => if (x > 3) Some(true) else None))
println((None:Option[Int]) flatMap (x => if (x > 3) Some(true) else None))

println(Some(4) flatMap2 (x => if (x > 3) Some(true) else None))
println(Some(3) flatMap2 (x => if (x > 3) Some(true) else None))
println((None:Option[Int]) flatMap2 (x => if (x > 3) Some(true) else None))

println(Some(3) filter (_ > 3))
println(Some(3) filter (_ > 2))
println((None:Option[Int]) filter (_ > 2))

println(None getOrElse 3)
println(Some(4) getOrElse 3)





