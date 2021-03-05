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


case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = this.run(s)
      f(a).run(s2)
    })

  def map[B](f: A => B): State[S, B] =
    this.flatMap(a => State.unit(f(a)))

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    this.flatMap(a => rb.flatMap(b => State.unit(f(a, b))))
}


object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(State.unit[S, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))
}

type Rand[A] = State[RNG, A]

val int: Rand[Int] = State(_.nextInt)

def nonNegativeInt(rng: RNG): (Int, RNG) = {

  val (value, nextInt) = int.run(rng)
  if (value < 0) nonNegativeInt(nextInt) else (value, nextInt)
}

def double: Rand[Double] = State(nonNegativeInt).map(_ / (Int.MaxValue.toDouble + 1))


def nonNegativeLessThan(n: Int): Rand[Int] = State(nonNegativeInt).flatMap(i => {
  val mod = i % n
  if (i + (n-1) - mod >= 0) State.unit(mod) else nonNegativeLessThan(n)
})

def rollDie: Rand[Int] = nonNegativeLessThan(6).map(_ + 1)

println(double.run(SimpleRNG(42)))
println(State.sequence(List.fill(32)(rollDie)).run(SimpleRNG(42)))

