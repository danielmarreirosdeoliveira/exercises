sealed trait Input
case object Coin extends Input
case object Turn extends Input


case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = this run s
      f(a) run s2
    })

  def map[B](f: A => B): State[S, B] =
    this flatMap (a => State.unit(f(a)))

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    this flatMap (a => rb.flatMap(b => State.unit(f(a, b))))
}


object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(State.unit[S, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))
}


case class Machine(locked: Boolean, coins: Int, candies: Int)


def simulateMachine = {
  input: Input =>
    m: Machine => { println(":" + input + ":: " + m)
      (input, m) match {
        case (_, Machine(_, _, 0)) => m

        case (Coin, Machine(false, _, _)) => m
        case (Coin, Machine(true, coins, candies)) => Machine(locked = false, coins + 1, candies)

        case (Turn, Machine(true, _, _)) => m
        case (Turn, Machine(false, coins, candies)) => Machine(locked = true, coins, candies - 1)
      }
    }
}


def get[S]: State[S, S] = State(s => (s, s))

def set[S](s: S): State[S, Unit] = State(_ => ((), s))

def modify[S](f: S => S): State[S, Unit] =
//  get.flatMap((s: S) => set(f(s)))
 for {
    s <- get
    _ <- set(f(s))
  } yield ()

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
//  State.sequence(inputs.map(input => modify(simulateMachine(input)))).flatMap(_ => get.map(s => (s.coins, s.candies)))
  for {
    _ <- State.sequence(inputs map (modify[Machine] _ compose simulateMachine))
    s <- get
  } yield (s.coins, s.candies)


val seq = simulateMachine(List(Coin, Turn, Turn, Coin, Coin, Turn))

println(seq.run(Machine(locked = true, 0, 2))._1)






