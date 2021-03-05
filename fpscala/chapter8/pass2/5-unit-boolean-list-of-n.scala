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

object State {

  def unit[A](a: A) = State((rng: RNG) => (a, rng))
}

def flatMap[A,B](f: State[RNG, A])(g: A => State[RNG, B]): State[RNG, B] = State(rng => {
  val (a, rng2) = f.run(rng)
  g(a).run(rng2)
})

def map[A, B](s: State[RNG, A])(f: A => B): State[RNG, B] =
  flatMap(s)(a => State.unit(f(a)))

def map2[A, B, C](ra: State[RNG, A], rb: State[RNG, B])(f: (A, B) => C): State[RNG, C] =
  State(rng => {
    val (a, rng2) = ra.run(rng)
    val (b, rng3) = rb.run(rng2)
    (f(a, b), rng3)
  })

def sequence[A](fs: List[State[RNG, A]]): State[RNG, List[A]] =
  fs.foldRight(State.unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

//

val int: RNG => (Int, RNG) = _.nextInt

def nonNegativeInt(rng: RNG): (Int, RNG) = {

  val (value, nextInt) = int(rng)
  if (value < 0) nonNegativeInt(nextInt) else (value, nextInt)
}

def nonNegativeLessThan(n: Int): State[RNG, Int] = {
  flatMap(State(nonNegativeInt))(i => {
    val mod = i % n
    if (i + (n-1) - mod >= 0) State.unit(mod) else nonNegativeLessThan(n)
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

def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

def boolean: Gen[Boolean] = {
  Gen(map(nonNegativeLessThan(2))(i => if (i == 1) true else false))
}

def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
  Gen(sequence(List.fill(n)(g.sample)))
}

println(unit(3).sample.run(SimpleRNG(42)))
println(boolean.sample.run(SimpleRNG(42)))
println(listOfN(13, boolean).sample.run(SimpleRNG(42)))


