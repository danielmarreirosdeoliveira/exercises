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

def pairIntDouble(rng: RNG): ((Int, Double), RNG) = {

  val (d, rng2) = double(rng)
  val (i, nextRng) = nonNegativeInt(rng2)

  ((i, d), nextRng)
}


def pairDoubleInt(rng: RNG): ((Double, Int), RNG) = {

  val (d, rng2) = double(rng)
  val (i, nextRng) = nonNegativeInt(rng2)

  ((d, i), nextRng)
}


def tripleDouble(rng: RNG): ((Double, Double, Double), RNG) = {

  val (d1, rng2) = double(rng)
  val (d2, rng3) = double(rng2)
  val (d3, nextRng) = double(rng3)

  ((d1, d2, d3), nextRng)
}


def printValues[A](f: RNG => (A, RNG), n: Int, rng: RNG): Unit = {

  def go(i: Int, rng: RNG): Unit = {
    if (i < 1) return
    val (value, rng2) = f(rng)
    println(value)
    go(i - 1, rng2)
  }
  go(n, rng)
}

printValues(pairIntDouble, 2, SimpleRNG(42))
printValues(pairDoubleInt, 2, SimpleRNG(42))
printValues(tripleDouble, 2, SimpleRNG(42))