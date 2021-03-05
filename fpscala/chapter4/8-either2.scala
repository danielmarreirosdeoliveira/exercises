sealed trait Either2[+A] {

  def flatMap[B](f: A => Either2[B]): Either2[B] = this match {
    case Right2(a) => f(a)
    case Left2(e) => Left2(e)
  }

  def map[B](f: A => B): Either2[B] =
    flatMap(a => Right2(f(a)))


  def map2[B, C](b: Either2[B])(f: (A, B) => C): Either2[C] =
    this match {
      case Left2(ll) => b match {
        case Right2(_) => Left2(ll)
        case Left2(e) => Left2(e:::ll)
      }
      case Right2(a) => b.flatMap(bb => Right2(f(a,bb)))
    }
}
case class Left2[+A](value: List[String]) extends Either2[A]
case class Right2[+A](valu: A) extends Either2[A]


// 3 / 0 => zerodivision - int => float
def divide3by(a: Int): Either2[Float] =
  if (a == 0) Left2(List("zerodivision"))
  else Right2(3 / a)

def times2with(a: Int): Int =
  2 * a

//println(Left2(List("error")).map2(Left2(List("error2")))((a:Int,b:Int)=>a*b))
//println(Left2(List("error")).flatMap(divide3by))
//println(Right2(3).flatMap(divide3by))
//println(Right2(0).flatMap(divide3by))


def traverse[A, B](as: List[A])(f: A => Either2[B]): Either2[List[B]] =
  for {
    l <- as.foldLeft(Right2(Nil):Either2[List[B]])((elb, a) =>
      elb.map2(f(a))((el, fa) => fa::el)
    )} yield l.reverse


println(traverse(List(1,2,3))(x => if (x==4) Left2(List("no 4 allowed")) else Right2(x)))
println(traverse(List(1,4,3))(x => if (x==4) Left2(List("no 4 allowed")) else Right2(x)))
println(traverse(List(1,4,5))(x => if (x==4) Left2(List("no 4 allowed")) else Right2(x)))
println(traverse(List(1,4,4,5))(x => if (x==4) Left2(List("no 4 allowed")) else Right2(x)))
println(traverse(List(1,4,4,5,4))(x => if (x==4) Left2(List("no 4 allowed")) else Right2(x)))

//println(Left2(List("error")).map(times2with))
//println(Right2(3).map(times2with))
//println(Right2(0).map(times2with))


