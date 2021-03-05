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

type Rand[+A] = RNG => (A, RNG)

def int(rng: RNG): (Int, RNG) = {

  val (i1, rng1) = rng.nextInt
  if (i1 < 0) int(rng1)
  else (i1,rng1)
}

def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => rng => (f(a),rng))

def double: Rand[Double] =
  map[Int,Double](int)(i => i % 1000 / 1000.0)

println(double(SimpleRNG(42)))

def map2ViaFlatMap[A, B, C](sa: Rand[A], sb: Rand[B])(f: (A, B) => C): Rand[C] =
  flatMap(sa)(a => rng2 => {
    val (d, rng3) = sb(rng2)
    (f(a,d),rng3)
  })

def both[A,B](sa: Rand[A], sb: Rand[B]): Rand[(A,B)] =
  map2ViaFlatMap(sa,sb)((_, _))

val randIntDouble: Rand[(Int, Double)] =
  both(int, double)

println(randIntDouble(SimpleRNG(42)))