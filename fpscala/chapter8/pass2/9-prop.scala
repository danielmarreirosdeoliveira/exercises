trait Stream[+A] {

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h,t) => Stream.cons(f(h), t))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }
}

// String interpolation syntax. A string starting with `s"` can refer to
// a Scala value `v` as `$v` or `${v}` in the string.
// This will be expanded to `v.toString` by the Scala compiler.
def buildMsg[A](s: A, e: Exception): String =
  s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

//////


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

def doubleViaMap: State[RNG, Double] =
  map(State(nonNegativeInt))(_ / (Int.MaxValue.toDouble + 1))

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

  def double: Gen[Double] = {
    Gen(doubleViaMap)
  }
}

def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
  Gen.boolean flatMap (b => if (b) g1 else g2)

def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
  Gen.double flatMap (d => {

    val (l, r, ratio) =
      if (g1._2 < g2._2) (g1._1, g2._1, g1._2 / g2._2)
      else (g2._1, g1._1, g2._2 / g1._2)

    if (d < ratio) l else r
  })
}

// // // //

import Prop._
case class Prop(run: (TestCases, RNG) => Result) {

}

object Prop {

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }


  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map(
      (generated, testCasesRun) => Passed
    )

//      .find(_.isFalsified).getOrElse(Passed)
  }
}



//def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
//  (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
//    case (a, i) => try {
//      Passed
//    } catch {
//      case e: Exception => Falsified("no", 10)
//    }
//  }.find(_.isFalsified).getOrElse(Passed)
//}
