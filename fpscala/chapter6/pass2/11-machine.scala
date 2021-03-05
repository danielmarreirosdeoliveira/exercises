sealed trait Input
case object Coin extends Input
case object Turn extends Input

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

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

case class Machine(locked: Boolean, candies: Int, coins: Int) { }

for {
  _ <- State.modify(s => s).run(Machine(true, 3 , 4))
  m <- State.get
} yield (m)
