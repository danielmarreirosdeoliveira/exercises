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

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](g: A => State[S, B]): State[S, B] = State((rng: S) => {
    val (a, rng2) = this run rng
    g(a) run rng2
  })
}

object State {

  def unit[A](a: A) = State((rng: RNG) => (a, rng))
}



def map[A, B](s: State[RNG, A])(f: A => B): State[RNG, B] =
  s flatMap (a => State.unit(f(a)))

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
  State(nonNegativeInt) flatMap (i => {
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

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(n: Int): Gen[List[A]] = {
    Gen(sequence(List.fill(n)(sample)))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (s => listOfN(s))
}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(intWithinRange(start, stopExclusive))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = {
    Gen(map(nonNegativeLessThan(2))(i => if (i == 1) true else false))
  }
}

def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
  Gen.boolean flatMap (b => if (b) g1 else g2)


val c1 = Gen.choose(3, 7)
val c2 = Gen.choose(17, 28)


println(union(c1, c2).listOfN(22).sample.run(SimpleRNG(42)))

