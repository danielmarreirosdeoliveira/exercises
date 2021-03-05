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



def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
  for {
    l <- as.foldLeft(Right(Nil):Either[E, List[B]])((elb, a) =>
      elb.map2(f(a))((el, fa) => fa::el)
  )} yield l.reverse

def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
  traverse(as)(a => a.flatMap(a => Right(a)))


println(traverse(List(1,2,3))(x => if (x==4) Left("no 4 allowed") else Right(x)))
println(traverse(List(1,2,4))(x => if (x==4) Left("no 4 allowed") else Right(x)))

println(sequence(List(Right("1"),Right("2"))))
println(sequence(List(Right("1"),Left("E2"))))
println(sequence(List(Left("E1"),Right("2"))))
println(sequence(List(Left("E1"),Left("E2"))))
println(sequence(List(Left("E1"),Left("E2"), Left("E3"))))

