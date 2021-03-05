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

def unit[A](a: A): Rand[A] =
  rng => (a, rng)

def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
  val (a, rng2) = f(rng)
  g(a)(rng2)
}

def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => unit(f(a)))

def map2[A,B,C](sa: Rand[A], sb: Rand[B])(f: (A, B) => C): Rand[C] =
  flatMap(sa)(saa => map(sb)(sbb => f(saa, sbb)))

def nonNegativeLessThan(n: Int): Rand[Int] = {
  flatMap(nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  })
}

def roll(count: Int)(rng: RNG) = {

  def go(n: Int, rng: RNG): Unit = {

    val (va, rng2) = rollDie(rng)
    println(va)
    if (n > 1) go(n-1,rng2)
  }

  go(count, rng)
}

def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

roll(40)(SimpleRNG(42))



