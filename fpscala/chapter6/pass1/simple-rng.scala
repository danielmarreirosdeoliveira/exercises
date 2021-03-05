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

def go(n: Int): Unit = {

  def _go(n: Int,r: RNG): Unit = {
    if (n > 0) {
      val (x, rn) = r.nextInt
      println(x)
      _go(n - 1, rn)
    }
  }
  _go(n,SimpleRNG(42))
}

go(4)
