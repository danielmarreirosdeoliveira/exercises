
trait RNG  {
  def next: (Int,RNG)
}

type Rand[+A] = RNG => (A, RNG)

case class SimpleRNG(seed: Long) extends RNG {

  override def next: (Int, SimpleRNG) = {

    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
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

def ints(n: Int): Rand[List[Int]] = {

  def _go(n: Int,z: List[Int],rng:RNG): (List[Int],RNG) = {
    if (n > 0) {
      val (i, rng2) = positiveNr(rng)
      _go(n - 1, i::z,rng2)
    } else {
      (z,rng)
    }
  }

  (rng:RNG) => _go(n,Nil:List[Int],rng)
}



val (list1,rng1) = ints(1)(SimpleRNG(42))

println(list1)