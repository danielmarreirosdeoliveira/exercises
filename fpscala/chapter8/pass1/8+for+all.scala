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

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
}

sealed trait Result {def isFalsified: Boolean}
case object Passed extends Result {def isFalsified = false}
case class Falsified(failure: Prop.FailedCase, successes: Prop.SuccessCount) extends Result {def isFalsified = true}

case class Prop(run: (Prop.TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop((i: Int, rng: RNG) => {

    this.run(i, rng) match {
      case Passed => p.run(i, rng) match {
        case Passed => Passed
        case Falsified(f, s) => Falsified(f, s)
      }
      case Falsified(f, s) => Falsified(f, s)
    }
  })
}

import Stream._
/*
def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop (
  (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
    case (a, i) => try {
      if (f(a)) Passed else Falsified(a.toString, i)
    } catch { case e: Exception => Falsified("exc", i) }
  }.find(_.isFalsified).getOrElse(Passed)
)

def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
  Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

forAll(intList)(a => {
  println("!",a)
  if (a.length > 2) false
  true
}).run((10,SimpleRNG(42)))
*/


val intList = choose(0, 100).listOfN(choose(1, 3))
val (l1, rng2) = intList.sample.run(SimpleRNG(42))
val (l2, rng3) = intList.sample.run(rng2)
val (l3, rng4) = intList.sample.run(rng3)
val (l4, rng5) = intList.sample.run(rng4)

println(l1)
println(l2)
println(l3)
println(l4)





