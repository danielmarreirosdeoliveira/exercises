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

def rndPair(rng:RNG[Int]): ((Int,Int), RNG[Int]) = {

  val (i1,rng2) = rng.next
  val (i2,rng3) = rng2.next

  ((i1,i2),rng3)
}

def go(n: Int): Unit = {

  def _go(n: Int,r: RNG[Int]): Unit = {
    if (n > 0) {
      val ((x,y), rn) = rndPair(r)
      println(x)
      println(y)
      _go(n - 1, rn)
    }

  }
  _go(n,SimpleRNG(42))
}

go(2)
