sealed trait Either[+A] {

  def map2[B, C](b: Either[B])(f: (A, B) => C): Either[C] =
    this match {
      case Error(e) => b match {
        case Error(ee) => Errors(e::ee::Nil)
        case Errors(es) => Errors(e::es)
        case Right(r) => Error(e)
      }
      case Errors(es) => b match {
        case Error(e) => Errors(e::es)
        case Errors(ess) => Errors(ess:::es)
        case Right(rr) => Errors(es)
      }
      case Right(r) => b match {
        case Error(e) => Error(e)
        case Errors(es) => Errors(es)
        case Right(rr) => Right(f(r,rr))
      }
    }
}
case class Error[+A](value: String) extends Either[A]
case class Errors[+A](value: List[String]) extends Either[A]
case class Right[+A](valu: A) extends Either[A]

println(Right(1).map2(Errors(List("error2")))((a:Int,b:Int)=>a*b))
println(Right(1).map2(Error("error2"))((a:Int,b:Int)=>a*b))
println(Error("error").map2(Error("error2"))((a:Int,b:Int)=>a*b))

def traverse[A, B](as: List[A])(f: A => Either[B]): Either[List[B]] =
  as.foldLeft(Right(Nil):Either[List[B]])((elb, a) =>
    elb.map2(f(a))((el, fa) => fa::el))


println(traverse(List(1,2,3))(x => if (x==4) Error("no 4 allowed") else Right(x)))
println(traverse(List(1,4,3))(x => if (x==4) Error("no 4 allowed") else Right(x)))
println(traverse(List(1,4,5))(x => if (x==4) Error("no 4 allowed") else Right(x)))
println(traverse(List(1,4,4,5))(x => if (x==4) Error("no 4 allowed") else Right(x)))
println(traverse(List(1,4,4,5,4))(x => if (x==4) Error("no 4 allowed") else Right(x)))




