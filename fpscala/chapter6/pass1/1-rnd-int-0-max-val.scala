trait RNG[A]  {
  def next: (A,RNG[A])
}

case class SimpleRNG(seed: Long) extends RNG[Int] {

  override def next: (Int, SimpleRNG) = {

    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

def positiveNr(rng:RNG[Int]): (Int,RNG[Int]) = {
  val (value, rn) = rng.next
  if (value <= Int.MinValue) {
    positiveNr(rn)
  } else if (value < 0 && value > Int.MinValue) {
    (-value, rn)
  } else if (value > Int.MaxValue) {
    positiveNr(rn)
  } else {
    (value,rn)
  }
}

def go(n: Int): Unit = {
  def _go(n: Int,rng: RNG[Int]): Unit = {
    if (n > 0) {
      val (value, rng2) = positiveNr(rng)
      println(value)
      _go(n - 1, rng2)
    }
  }
  _go(n,SimpleRNG(42))
}

println(go(100000))
