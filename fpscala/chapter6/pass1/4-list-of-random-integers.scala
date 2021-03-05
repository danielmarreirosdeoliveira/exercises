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

def ints(n: Int)(rng:RNG): (List[Int],RNG) = {
  def _go(n: Int,rng: RNG,z: List[Int]): (List[Int],RNG) = {
    if (n > 0) {
      val (i, rng2) = positiveNr(rng)
      _go(n - 1, rng2, i::z)
    } else {
      (z,rng)
    }
  }
  _go(n,rng,Nil:List[Int])
}

val (list1,rng1) = ints(1)(SimpleRNG(42))
val (list2,rng2) = ints(1)(rng1)
println(list1)
println(list2)