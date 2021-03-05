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

  if (value == Int.MinValue) nonNegativeInt(rng2)
  else if (value < 0) (-value, rng2)
  else (value, rng2)
}

def double(rng: RNG): (Double, RNG) = {

  val (value, rng2) = rng.nextInt
  if (value > 9999999 || value < 0) double(rng2)
  else ((value.toDouble / 10000000.0), rng2)
}

def loop(f: RNG => (Double, RNG), n: Int, rng: RNG) = {

  def go(n: Int, rng: RNG): Unit = {
    val (value, rng2) = f(rng)
    println(":"+value)
    if (n > 0) go(n-1, rng2)
  }
  go(n, rng)
}

loop(double, 100, SimpleRNG(42))