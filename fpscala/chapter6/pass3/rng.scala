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

println(SimpleRNG(42).nextInt)


case class RandPair(rng :RNG) {
  def next: ((Int,Int),RandPair) = {
    val (i1,rng1) = rng.nextInt
    val (i2,rng2) = rng1.nextInt
    ((i1,i2),RandPair(rng2))
  }
}

val (is1,rp1) = RandPair(SimpleRNG(42)).next
println(is1)
val (is2,rp2) = rp1.next
println(is2)