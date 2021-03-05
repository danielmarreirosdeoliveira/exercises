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
  if (value > 999 || value < 0) double(rng2)
  else (value / 1000.0, rng2)
}


def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

  def go(l: (List[Int], RNG), n: Int): (List[Int],RNG) = {

    val (value, rng2) = l._2.nextInt

    if (n > 0) go((value::l._1,rng2), n-1)
    else (value::l._1, rng2)
  }

  go((List(),rng), count)
}

println(ints(5)(SimpleRNG(42)))


