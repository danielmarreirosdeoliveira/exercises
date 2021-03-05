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

type Rand[+A] = RNG => (A, RNG)

def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a, rng2) = s(rng)
    (f(a),rng2)
  }

def map2[A,B,C](sa: Rand[A], sb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng => {
    val (a, rng2) = sa(rng)
    val (b, rng3) = sb(rng2)
    (f(a,b),rng3)
  }

def nonNegativeInt: Rand[Int] = rng => {

  val (value, rng2) = rng.nextInt

  if (value == Int.MinValue) nonNegativeInt(rng2)
  else if (value < 0) (-value, rng2)
  else (value, rng2)
}

def positiveSmallerThan(n: Int)(rng: RNG): (Int, RNG) = {

  val (value, rng2) = rng.nextInt
  if (value > n || value < 0) positiveSmallerThan(n)(rng2)
  else (value, rng2)
}

def double: Rand[Double] =
  map(positiveSmallerThan(1000))(i => i / 1000.0)




def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
  map2(ra, rb)((_, _))

def randIntDouble: Rand[(Int, Double)] =
  both(nonNegativeInt, double)

def randDoubleInt: Rand[(Double, Int)] =
  both(double, nonNegativeInt)



val (a,rng2) = randIntDouble(SimpleRNG(42))
val (b,rng3) = randDoubleInt(rng2)

println(a)
println(b)

