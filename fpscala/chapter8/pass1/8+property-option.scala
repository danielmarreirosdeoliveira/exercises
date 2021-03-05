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

def fm[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = State(rng => {
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
  fm(nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n-1) - mod >= 0) u(mod) else nonNegativeLessThan(n)
  })
}
def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  fm(s)(a => u(f(a)))



/// ////// ///

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = {

    Gen(State(rng => {
      val (a, rng2) = this.sample.run(rng)
      f(a).sample.run(rng2)
    }))
  }

  def listOfN(n: Gen[Int]): Gen[List[A]] = {
    n.flatMap(ni =>
      Gen(State(rng => {

        def go(i: Int, l: List[A], rNG: RNG): (List[A], RNG) = {

          val (a, rng2) = this.sample.run(rNG)
          if (i > 0) go(i-1, a::l, rng2)
          else (l, rng2)
        }

        go(ni, Nil:List[A], rng)
      }))
    )
  }
}


def choose(start: Int, stopExclusive: Int): Gen[Int] = {
  Gen(
    map(nonNegativeLessThan(stopExclusive - start))(i => {
      i + start
    })
  )
}

def unit[A](a: => A): Gen[A] = {
  Gen(State(rng => (a, rng)))
}

def boolean: Gen[Boolean] = {
  Gen(
    map(nonNegativeLessThan(100))(i => i % 2 == 0)
  )
}


object Prop {

  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type Result = Option[(Prop.FailedCase, Prop.SuccessCount)]
}


case class Prop(run: Prop.TestCases => Prop.Result) {

  def &&(p: Prop): Prop = Prop((i: Int) => {

    this.run(i) match {
      case None => p.run(i) match {
        case None => None
        case Some(rr) => Some(rr)
      }
      case Some(r) => Some(r)
    }
  })
}


val prop = Prop(i => None)

val prop2 = Prop(i => Some(("a",i)))

val prop3 = Prop(i => Some(("b",i)))


println((prop && prop2 && prop3).run(2))