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
  a.flatMap[C](iA =>
  b.map[C](iB =>
  f(iA, iB)))
}

def map2_v2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  for {
    iA <- a
    iB <- b
  } yield f(iA,iB)
}

println(map2(Some(2),Some(3.0))((a,b)=>if ((a==2)&&(b==3.0)) true else false))
println(map2(Some(2),Some(3.0))((a,b)=>if ((a==1)&&(b==3.0)) true else false))
println(map2_v2(Some(2),Some(3.0))((a,b)=>if ((a==2)&&(b==3.0)) true else false))
println(map2_v2(Some(2),Some(3.0))((a,b)=>if ((a==1)&&(b==3.0)) true else false))


def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age * numberOfSpeedingTickets

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None }

def parseInsuranceRateQuote(
                           age: String,
                           numberOfSpeedingTickets: String
                           ): Option[Double] = {

  val optAge: Option[Int] = Try { age.toInt }
  val optTickets: Option[Int] = Try { numberOfSpeedingTickets.toInt }
  map2(optAge, optTickets)(insuranceRateQuote)
}

println(parseInsuranceRateQuote("a","1"))
println(parseInsuranceRateQuote("2","a"))
println(parseInsuranceRateQuote("2","3"))