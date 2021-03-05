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


def intDouble(rng: RNG): ((Int, Double), RNG) = {
  val (intValue, rng2) = nonNegativeInt(rng)
  val (doubleValue, rng3) = double(rng2)
  ((intValue, doubleValue), rng3)
}


def doubleInt(rng: RNG): ((Double, Int), RNG) = {
  val (doubleValue, rng2) = double(rng)
  val (intValue, rng3) = nonNegativeInt(rng2)
  ((doubleValue, intValue), rng3)
}


def double3(rng: RNG): ((Double, Double, Double), RNG) = {
  val (doubleValue1, rng2) = double(rng)
  val (doubleValue2, rng3) = double(rng2)
  val (doubleValue3, rng4) = double(rng3)
  ((doubleValue1, doubleValue2, doubleValue3), rng4)
}

val (a,rng2) = intDouble(SimpleRNG(42))
val (b,rng3) = doubleInt(rng2)
val (c,rng4) = double3(rng3)

println(a)
println(b)
println(c)