trait RNG  {
  def next: (Int,RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  override def next: (Int, SimpleRNG) = {

    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

def doubleBetween0and1(rng:RNG): (Double,RNG) = {
  val (value, rn) = rng.next
  if (value < 0) {
    doubleBetween0and1(rn)
  } else if (value > 100000) {
    doubleBetween0and1(rn)
  } else {
    (1.0 / makePlain(value.toDouble),rn)
  }
}

def go(n: Int): Unit = {
  def _go(n: Int,rng: RNG): Unit = {
    if (n > 0) {
      val (value, rng2) = doubleBetween0and1(rng)
      println(value)
      _go(n - 1, rng2)
    }
  }
  _go(n,SimpleRNG(42))
}

def makePlain(n: Double): Double = {
  if ((n / 10) >= 1.0) {
    makePlain(n / 10)
  } else {
    n
  }
}

println(go(10))