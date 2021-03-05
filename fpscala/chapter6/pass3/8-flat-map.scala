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


def unit[A](a: A): Rand[A] =
  rng => (a, rng)

def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {

  val (a, rng2) = f(rng)
  g(a)(rng2)
}

def nonNegativeLessThan(n: Int): Rand[Int] = {
  flatMap(nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  })
}

val (a1,rng2) = nonNegativeLessThan(3)(SimpleRNG(42))
val (a2,rng3) = nonNegativeLessThan(3)(rng2)
val (a3,rng4) = nonNegativeLessThan(3)(rng3)
val (a4,rng5) = nonNegativeLessThan(3)(rng4)

println(a1)
println(a2)
println(a3)
println(a4)


