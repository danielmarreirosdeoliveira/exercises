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

def nonNegativeInt(rng: RNG): (Int, RNG) = {

  val (i1, rng1) = rng.nextInt
  if (i1 < 0) nonNegativeInt(rng1)
  else (i1, rng1)
}

def go(n: Int): Unit = {
  def _go(n: Int,rng: RNG): Unit = {
    if (n > 0) {
      val (value, rng2) = nonNegativeInt(rng)
      println(value)
      _go(n - 1, rng2)
    }
  }
  _go(n,SimpleRNG(42))
}

println(go(14))
