trait RNG  {def nextInt: (Int,RNG)}
case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class State[S, +A](run: S => (A, S)) {}
type Rand[+A] = State[RNG, A]

def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = State(rng => {
  val (a, rng2) = f.run(rng)
  g(a).run(rng2)
})
def u[A](a: A): Rand[A] = State(rng => (a, rng))
def nonNegativeInt: Rand[Int] = State(rng => {

  val (value, rng2) = rng.nextInt

  if (value == Int.MinValue) nonNegativeInt.run(rng2)
  else if (value < 0) (-value, rng2)
  else (value, rng2)
})
def nonNegativeLessThan(n: Int): Rand[Int] = {
  flatMap(nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n-1) - mod >= 0) u(mod) else nonNegativeLessThan(n)
  })
}
def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => u(f(a)))



/// ////// ///

case class Gen[A](sample: State[RNG, A])

def choose(start: Int, stopExclusive: Int): Gen[Int] = {
  Gen(
    map(nonNegativeLessThan(stopExclusive - start))(i => {
      i + start
    })
  )
}

//

def unit[A](a: => A): Gen[A] = {
  Gen(State(rng => (a, rng)))
}

def boolean: Gen[Boolean] = {
  Gen(
    map(nonNegativeLessThan(100))(i => i % 2 == 0)
  )
}

def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
  Gen(
    State(
      rng => {

        def go(i: Int, l: List[A], rNG: RNG): (List[A], RNG) = {

          val (a, rng2) = g.sample.run(rNG)
          if (i > 0) go(i-1, a::l, rng2)
          else (l, rng2)
        }

        go(n, Nil:List[A], rng)
      }
    )
  )
}

val _l = listOfN(5, boolean).sample
val (i, rng) = _l.run(SimpleRNG(42))
println(i)