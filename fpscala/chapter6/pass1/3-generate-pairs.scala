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



def positiveNr(rng:RNG): (Int,RNG) = {
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

def makePlain(n: Double): Double = {
  if ((n / 10) >= 1.0) {
    makePlain(n / 10)
  } else {
    n
  }
}

def generateIntDoublePair(rng: RNG): ((Int,Double),RNG) = {
  val (i,rng1) = positiveNr(rng)
  val (d,rng2) = doubleBetween0and1(rng1)
  ((i,d),rng2)
}

def go(n: Int): Unit = {
  def _go(n: Int,rng: RNG): Unit = {
    if (n > 0) {
      val ((i,d), rng2) = generateIntDoublePair(rng)
      println(i)
      println(d)
      _go(n - 1, rng2)
    }
  }
  _go(n,SimpleRNG(42))
}

go(20)