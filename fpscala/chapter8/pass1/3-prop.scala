object Prop {

  type FailedCase = String
  type SuccessCount = Int
}


trait Prop {

//  def check: Either[(Prop.FailedCase, Prop.SuccessCount), Prop.SuccessCount]
  def check: Boolean

  def &&(p: Prop): Prop = {

    Property(this.check && p.check)
  }

  def ||(p: Prop): Prop = {

    Property(this.check || p.check)
  }
}



case class Property(check: Boolean) extends Prop {

}


val prop = Property(false)
val other = Property(true)
val result = prop || other

println(result)