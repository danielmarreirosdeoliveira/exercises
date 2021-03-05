
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

def positiveNr: Rand[Int] = {
  rng => {
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
}

//val (i,r1) = positiveNr(SimpleRNG(42))
//val (i2,r2) = positiveNr(r1)
//println(i)
//println(i2)

def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a,rng2) = s(rng)
    (f(a), rng2)
  }

def nonNegativeEven: Rand[Int] =
  map(positiveNr)(i => i - i % 2)

val (i,r1) = nonNegativeEven(SimpleRNG(42))
val (i2,r2) = nonNegativeEven(r1)
println(i)
println(i2)

