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
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

def map2[EE, A, B, C](a: Either[EE, A], b: Either[EE, B])(f: (A, B) => C): Either[List[EE], C] = {
  a match {
    case Right(aa) => b match {
      case Right(bb) => Right(f(aa,bb)): Either[List[EE], C]
      case Left(e) => Left(List(e))
    }
    case Left(e) => b match {
      case Right(bb) => Left(List(e))
      case Left(ee) => Left(List(e, ee))
    }
  }
}


case class Person(name: Name, age: Age)
case class Name(value: String)
case class Age(value: Int)

def mkName(name: String): Either[String, Name] =
  if (name == "" || name == null) Left("no name")
  else Right(Name(name))

def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("age out of range")
  else Right(Age(age))

def mkPerson(name: String, age: Int): Either[List[String], Person] =
  map2(mkName(name), mkAge(age))((n, a)=>Person(n, a))

println(mkPerson("",13))
println(mkPerson("",-1))
println(mkPerson("a",-1))
println(mkPerson("a",1))

