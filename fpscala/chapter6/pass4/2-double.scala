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

  val (value, nextInt) = rng.nextInt
  if (value < 0) nonNegativeInt(nextInt) else (value, nextInt)
}


def double(rng: RNG): (Double, RNG) = {

  val (value, nextRNG) = nonNegativeInt(rng)

  if (value == Int.MaxValue) return double(nextRNG)
  if (value == 0) return (0.0, nextRNG)

  (value.toDouble / Int.MaxValue, nextRNG)
}


def printValues(f: RNG => (Double, RNG), n: Int, rng: RNG): Unit = {

  def go(n: Int, rng: RNG): Unit = {
    val (value, rng2) = f(rng)
    println(value)
    if (n > 0) go(n-1, rng2)
  }
  go(n, rng)
}

printValues(double, 10, SimpleRNG(42))