trait RNG  {
  def nextInt: (Int,RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  override def nextInt: (Int, RNG) = {

    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class State[S, +A](run: S => (A, S)) {}

object State {
  def unit[S, A](a: A): State[S, A] =
    State((s: S) => {
      (a, s)
    })
}

object Prop {

}

trait Prop {
  type FailedCase = String
  type SuccessCount = Int

  def check: Either[(FailedCase, SuccessCount),SuccessCount]
}

case class Gen(sample: State[RNG, Int])


def intWithinRange(rng: RNG, start: Int, stopExclusive: Int): (Int, RNG) = {

  val (i1, rng1) = rng.nextInt
  if (i1 >= stopExclusive || i1 < start) intWithinRange(rng1, start, stopExclusive)
  else (i1, rng1)
}


def choose(start: Int, stopExclusive: Int): Gen = {
  Gen(State[RNG,Int](rng=> intWithinRange(rng, start, stopExclusive)))
}

//val s1 = State((r: RNG) => nonNegativeInt(r))
//val (i,r) = s1.run(SimpleRNG(42))
//val (i2,r2) = s1.run(r)
//println(i)
//println(i2)

println(choose(1, 100).sample.run(SimpleRNG(42)))


