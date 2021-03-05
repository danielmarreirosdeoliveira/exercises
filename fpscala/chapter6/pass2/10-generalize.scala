trait RNG  {
  def nextInt: (Int,RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  override def nextInt: (Int, RNG) = {

    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

type Rand[A] = State[RNG,A]
//type State[S, +A] = S => (A, S)
case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State((s: S) => {
      val (a, rng2) = this.run(s)
      g(a).run(rng2)
    })

  def map[B](f: A => B): State[S, B] =
    this.flatMap(a => State((r: S) => (f(a), r)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State((s: S) => {
      val (a, s2) = this.run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    })
}

object State {
  def sequence[S, A](as: List[State[S, A]], s: S): (List[A] ,S) =
    as.foldRight((Nil:List[A], s))((s, b) => {
      val (v, s2) = s.run(b._2)
      (v::b._1, s2)
    })

  def unit[S, A](a: A): State[S, A] =
    State((s: S) => {
      (a, s)
    })
}

val int: State[RNG, Int] =
    State((rng: RNG) => {
      val (i1, rng1) = rng.nextInt
      if (i1 < 0) int.run(rng1)
      else (i1,rng1)
    })

val double: State[RNG, Double] =
  int.map(i => i % 1000 / 1000.0)

val (i1,rng2) = double.run(SimpleRNG(42))
val (i2,rng3) = double.run(rng2)
println(i1)
println(i2)

def both[S, A, B](sa: State[S, A], sb: State[S, B]): State[S, (A, B)] =
  sa.map2(sb)((_, _))

val randIntDouble: Rand[(Int, Double)] =
  both(int, double)

println(randIntDouble.run(SimpleRNG(42)))

println(State.sequence[RNG, Int](List(int, int), SimpleRNG(42)))

println(State.unit(4).run(SimpleRNG(42)))