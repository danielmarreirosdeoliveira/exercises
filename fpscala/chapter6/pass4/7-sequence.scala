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

def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

def nonNegativeInt(rng: RNG): (Int, RNG) = {

  val (value, nextInt) = int(rng)
  if (value < 0) nonNegativeInt(nextInt) else (value, nextInt)
}

def double: Rand[Double] =
  map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))


def ints(n: Int) = sequence[Int](List.fill(n)(int))



println(sequence(List.fill(5)(double))(SimpleRNG(42)))
println(ints(3)(SimpleRNG(42)))




