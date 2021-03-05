object Prop {

  type FailedCase = String
  type SuccessCount = Int

  type Result = Either[(Prop.FailedCase, Prop.SuccessCount), Prop.SuccessCount]
}


trait Prop {

  def check: Prop.Result

  def &&(p: Prop): Prop = {

    check match {
      case Left(l) => Property(Left(l))
      case Right(r) =>
        p.check match {
          case Left(l) => Property(Left((l._1,l._2 + r)))
          case Right(rr) => Property(Right(rr + r))
        }
    }
  }
}



case class Property(check: Prop.Result) extends Prop {}


val prop = Property(Right(3))
val prop2 = Property(Right(6))
val other = Property(Left(("a", 4)))
val another = Property(Left(("b", 4)))
val result = prop && prop2 && other && another

println(result)