sealed trait Either[+E,+A] {

  def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }
  def map[B](f: A => B): Either[E, B] =
    flatMap(a => Right(f(a)))

  def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(e) => Right(e)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa,bb)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

def Try[A](a: => A): Either[Exception, A] =
  try Right(a)
  catch { case e: Exception => Left(e) }

def parseInsuranceRateQuote(
      age: String,
      numberOfSpeedingTickets: String
    ): Either[Exception, Double] =
  for {
    a <- Try { age.toInt }
    tickets <- Try { numberOfSpeedingTickets.toInt }
  } yield a * tickets

println(parseInsuranceRateQuote("3","5"))
println(parseInsuranceRateQuote("no","5"))
println(parseInsuranceRateQuote("no","no"))

println(Right("hallo")
  .map(a => a + a))
println((Left(new Error("e")):Either[Error, String])
  .map(a => a +a))

println(Left(new Error("no")).map2(Right("yo"))((a: String,b: String)=>if ((a == "no") && (b == "yo")) "1"))
println(Right("no").map2(Right("yo"))((a: String,b: String)=>if ((a == "no") && (b == "yo")) "1"))
println(Right("no").map2(Left(new Error("yo")))((a: String,b: String)=>if ((a == "no") && (b == "yo")) "1"))

println(Right("right").orElse(Right("now")))
println(Left(new Error("NO")).orElse(Right("now")))
println(Left(new Error("NO")).orElse(Left(new Error("no"))))

println(Right("hallo")
  .flatMap(a => if (a == "Hallo") Right("There") else Left("Not")))
println(Right("hallo")
  .flatMap(a => if (a == "hallo") Right("There") else Left("Not")))
println((Left(new Error("e")):Either[Error, String])
  .flatMap(a => if (a == "Hallo") Right("There") else Left("Not")))