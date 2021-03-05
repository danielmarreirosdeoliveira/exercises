trait RNG {
  def nextInt: (Int, RNG)
}
case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class State[S, +A](run: S => (A, S)) {}

def unit[A](a: A) = State((rng: RNG) => (a, rng))

def flatMap[A,B](f: State[RNG, A])(g: A => State[RNG, B]): State[RNG, B] = State(rng => {
  val (a, rng2) = f.run(rng)
  g(a).run(rng2)
})

def map[A, B](s: State[RNG, A])(f: A => B): State[RNG, B] =
  flatMap(s)(a => unit(f(a)))

def map2[A, B, C](ra: State[RNG, A], rb: State[RNG, B])(f: (A, B) => C): State[RNG, C] =
  State(rng => {
    val (a, rng2) = ra.run(rng)
    val (b, rng3) = rb.run(rng2)
    (f(a, b), rng3)
  })

def sequence[A](fs: List[State[RNG, A]]): State[RNG, List[A]] =
  fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

//

val int: RNG => (Int, RNG) = _.nextInt

def nonNegativeInt(rng: RNG): (Int, RNG) = {

  val (value, nextInt) = int(rng)
  if (value < 0) nonNegativeInt(nextInt) else (value, nextInt)
}

def nonNegativeLessThan(n: Int): State[RNG, Int] = {
  flatMap(State(nonNegativeInt))(i => {
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  })
}

def intWithinRange(start: Int, stopExclusive: Int): State[RNG, Int] = {
  val range = stopExclusive - start
  map(nonNegativeLessThan(range))(i => i + start)
}

//println(sequence(List.fill(33)(intWithinRange(-1, 2))).run(SimpleRNG(42)))

/// ////// ///

case class Gen[A](sample: State[RNG, A])

def choose(start: Int, stopExclusive: Int): Gen[Int] = {

  Gen(intWithinRange(start, stopExclusive))
}

println(choose(-1, 2).sample.run(SimpleRNG(42)))


