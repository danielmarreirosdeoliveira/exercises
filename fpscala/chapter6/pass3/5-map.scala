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

def nonNegativeEven: Rand[Int] =
  map(nonNegativeInt)(i => i - i % 2)

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


def loop(f: RNG => (Double, RNG), n: Int, rng: RNG) = {

  def go(n: Int, rng: RNG): Unit = {
    val (value, rng2) = f(rng)
    println(":"+value)
    if (n > 0) go(n-1, rng2)
  }
  go(n, rng)
}

loop(double, 20, SimpleRNG(42))


