sealed trait Either[+E,+A]
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

def mean(xs: IndexedSeq[Double]): Either[String, Double] =
  if (xs.isEmpty)
    Left("mean of empty list!")
  else
    Right(xs.sum / xs.length)


println(mean(IndexedSeq(1.0,2.0,3.0)))
println(mean(IndexedSeq()))

def safeDiv(x: Int, y: Int): Either[Exception, Int] =
  try Right(x / y)
  catch { case e: Exception => Left(e) }

println(safeDiv(3, 4))
println(safeDiv(3, 0))

def Try[A](a: => A): Either[Exception, A] =
  try Right(a)
  catch { case e: Exception => Left(e) }

println(Try { 3 / 4 })
println(Try { 3 / 0 })