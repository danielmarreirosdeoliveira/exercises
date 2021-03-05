



//def listOf[A](a: Gen[A]): Gen[List[A]]
//def forAll[A](a: Gen[A])(f: A => Boolean): Prop


//object Prop {
//
//}

trait Prop {

  type FailedCase = String
  type SuccessCount = Int

  def check: Either[(FailedCase, SuccessCount), SuccessCount]
//  def check: Boolean
//  def &&(p: Prop): Prop

}


println("hello")



