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


case class Person(name: Name, age: Age)
case class Name(value: String)
case class Age(value: Int)

def mkName(name: String): Either[String, Name] =
  if (name == "" || name == null) Left("no name")
  else Right(Name(name))

def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("age out of range")
  else Right(Age(age))

def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))((n, a)=>Person(n, a))

println(mkPerson("",13))
println(mkPerson("",-1))
println(mkPerson("a",-1))
println(mkPerson("a",1))

