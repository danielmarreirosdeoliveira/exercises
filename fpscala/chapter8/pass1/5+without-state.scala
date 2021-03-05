trait RNG  {def nextInt: (Int,RNG)}
case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

type State[S, +A] = S => (A, S)
type Rand[+A] = State[RNG, A]

def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
  val (a, rng2) = f(rng)
  g(a)(rng2)
}
def u[A](a: A): Rand[A] = rng => (a, rng)
def nonNegativeInt: Rand[Int] = rng => {

  val (value, rng2) = rng.nextInt

  if (value == Int.MinValue) nonNegativeInt(rng2)
  else if (value < 0) (-value, rng2)
  else (value, rng2)
}
def nonNegativeLessThan(n: Int): Rand[Int] = {
  flatMap(nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n-1) - mod >= 0) u(mod) else nonNegativeLessThan(n)
  })
}
def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => u(f(a)))



/// ////// ///

case class Gen[A](sample: Rand[A])

def choose(start: Int, stopExclusive: Int): Gen[Int] = {
  Gen(
    map(nonNegativeLessThan(stopExclusive - start))(i => {
      i + start
    })
  )
}

//

def unit[A](a: => A): Gen[A] = {
  Gen(rng => (a, rng))
}

def boolean: Gen[Boolean] = {
  Gen(
    map(nonNegativeLessThan(100))(i => i % 2 == 0)
  )
}

def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
  Gen(
      rng => {

        def go(i: Int, l: List[A], rNG: RNG): (List[A], RNG) = {

          val (a, rng2) = g.sample(rNG)
          if (i > 0) go(i-1, a::l, rng2)
          else (l, rng2)
        }

        go(n, Nil:List[A], rng)
      }
  )
}

def pair[A](g: Gen[A]): Gen[(A, A)] = {
  Gen(
      rng => {
        val (list, rng2) = listOfN(2, g).sample(rng)
        ((list.head,list.tail.head), rng2)
      }
  )
}

val _p = pair(choose(0, 2)).sample
val (i, rng1) = _p(SimpleRNG(42))
val (i2, rng2) = _p(rng1)
println(i)
println(i2)