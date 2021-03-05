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

case class State[S,+A](run: S => (A,S)) {

  def flatMap[B](g: A => State[S,B]): State[S,B] = State(s => {
    val (a, s2) = this.run(s)
    g(a).run(s2)
  })

  def map[B](f: A => B): State[S,B] =
    this.flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S,B])(f: (A, B) => C): State[S,C] =
    for {
      saa <- this
      sbb <- sb
    } yield f(saa,sbb)
//    this.flatMap(saa => sb.map(sbb => f(saa,sbb)))
}

object State {

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = {

    def go(rl: State[S,List[A]], fs: List[State[S,A]]): State[S,List[A]] = fs match {

      case Nil => rl
      case (h :: t) => go(
        h.map2(rl)((a, b) => a::b),
        t)
    }
    go(unit[S,List[A]](Nil), fs)
  }

  def unit[S,A](a: A): State[S,A] =
    State(s => (a, s))
}

type Rand[+A] = State[RNG,A]

def nonNegativeInt: Rand[Int] = State((rng: RNG) => {

  val (value, rng2) = rng.nextInt

  if (value == Int.MinValue) nonNegativeInt.run(rng2)
  else if (value < 0) (-value, rng2)
  else (value, rng2)
})

// Why is this leading to stackoverflow while nonNegativeLessThan works just fine?
// for example when used in double
//def positiveSmallerThan(n: Int): Rand[Int] = State((rng: RNG) => {
//
//  val (value, rng2) = rng.nextInt
//  if (value > n || value < 0) positiveSmallerThan(n).run(rng2)
//  else (value, rng2)
//})

def double: Rand[Double] =
  nonNegativeLessThan(1000).map(i => i / 1000.0)

def nonNegativeLessThan(n: Int): Rand[Int] = {
  nonNegativeInt.flatMap(i => {
    val mod = i % n
    if (i + (n-1) - mod >= 0) State.unit(mod) else nonNegativeLessThan(n)
  })
}

def roll(count: Int)(rng: RNG) = {

  def go(n: Int, rng: RNG): Unit = {

    val (va, rng2) = rollDie.run(rng)
    println(va)
    if (n > 1) go(n-1,rng2)
  }

  go(count, rng)
}

def rollDie: Rand[Int] = nonNegativeLessThan(6).map(_ + 1)

roll(4)(SimpleRNG(42))

println(State.sequence(List(double, nonNegativeLessThan(5))).run(SimpleRNG(42)))

