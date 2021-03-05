trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}


def nonNegativeInt(rng: RNG): (Int, RNG) = {

  val (value, rng2) = rng.nextInt
  if (value < 0) nonNegativeInt(rng2) else (value, rng2)
}


val rng = SimpleRNG(42)
val (i1, rng1) = nonNegativeInt(rng)
val (i2, _) = nonNegativeInt(rng1)

println(i1)
println(i2)