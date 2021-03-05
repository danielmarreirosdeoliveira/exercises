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

def mkPerson(name: String, age: Int): Either[(String, String), Person] = {

  val madeName = mkName(name)
  val madeAge = mkAge(age)

  var e1 = ""
  var mdName: Name = null
  madeName match {
    case Left(er) => e1 = er
    case Right(r) => mdName = r
  }

  var e2 = ""
  var mdAge: Age = null
  madeAge match {
    case Left(er) => e2 = er
    case Right(r) => mdAge = r
  }

  if (e1 != "" && e2 != "") Left((e1,e2))
  else if (e1 != "") Left((e1,"")): Either[(String, String), Person]
  else if (e2 != "") Left(("",e2)): Either[(String, String), Person]
  else Right(Person(mdName, mdAge)): Either[(String, String), Person]
//
}

println(mkPerson("",13))
println(mkPerson("",-1))
println(mkPerson("a",-1))
println(mkPerson("a",1))

