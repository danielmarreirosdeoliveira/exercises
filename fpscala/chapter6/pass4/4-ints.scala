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

def ints(n: Int, rng: RNG): (List[Int], RNG) = {

  def go(i: Int, rng: RNG, as: List[Int]): (List[Int], RNG) = {
    if (i < 1) return (as, rng)
    val (intVal, nextRng) = rng.nextInt
    go(i - 1, nextRng, intVal::as)
  }
  go(n, rng, List[Int]())
}

println(ints(3, SimpleRNG(42))._1)



