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

def range01(rng: RNG): (Double, RNG) = {

  val (i1, rng1) = rng.nextInt
  if (i1 < 0) return range01(rng1)
  (i1 % 1000 / 1000.0, rng1)
}

def intDouble(rng: RNG): ((Int,Double), RNG) = {
  val (double, rng1) = range01(rng)
  val (int, rng2) = nonNegativeInt(rng1)
  ((int,double), rng2)
}
def go1(n: Int): Unit = {

  def _go(n: Int,r: RNG): Unit = {
    if (n > 0) {
      val (x, rn) = intDouble(r)
      println(x)
      _go(n - 1, rn)
    }
  }
  _go(n,SimpleRNG(42))
}
go1(10)
def double3(rng: RNG): ((Double, Double, Double), RNG) = {
  val (d1, rng1) = range01(rng)
  val (d2, rng2) = range01(rng1)
  val (d3, rng3) = range01(rng2)
  ((d1,d2,d3), rng3)
}
def go2(n: Int): Unit = {

  def _go(n: Int,r: RNG): Unit = {
    if (n > 0) {
      val (x, rn) = double3(r)
      println(x)
      _go(n - 1, rn)
    }
  }
  _go(n,SimpleRNG(42))
}
go2(10)