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

val int: Rand[Int] = _.nextInt

def unit[A](a: A): Rand[A] =
  rng => (a, rng)

def nonNegativeInt(rng: RNG): (Int, RNG) = {

  val (value, nextInt) = int(rng)
  if (value < 0) nonNegativeInt(nextInt) else (value, nextInt)
}

def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] =
  rng => {
    val (a, rng2) = s(rng)
    f(a)(rng2)
  }

def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => unit(f(a)))

def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {
  val mod = i % n
  if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
})

def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

println(sequence(List.fill(32)(rollDie))(SimpleRNG(42)))

