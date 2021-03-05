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





def set[S](s: S): State[S, Unit] = State(_ => ((), s))
def get[S]: State[S, S] = State(s => (s, s))


def modify[S](f: S => S): State[S, Unit] =
  get.flatMap((got: S) => set(f(got)))



sealed trait Input
case object Coin extends Input
case object Turn extends Input


case class Machine(locked: Boolean, candies: Int, coins: Int)


def coinFun(m:Machine): Machine = {

  if (m.candies == 0) return m

  if (m.candies > 0 && m.locked) {
    Machine(locked = false, candies = m.candies, coins = m.coins)
  } else m
}

def turnFun(m:Machine): Machine = {

  if (m.candies == 0) return m

  if (!m.locked && m.candies > 0) {
    Machine(locked = true, candies = m.candies - 1, coins = m.coins)
  } else m
}

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State((s: Machine) => {

    def go(ins: List[Input], m: Machine): Machine = {

      ins match {
        case input :: rest =>
          go(rest,input match {
            case Coin => coinFun(m)
            case Turn => turnFun(m)
          })
        case Nil =>  m
      }

    }

  val result = go(inputs, s)

  ((result.candies,result.coins),result)
})




println(
  simulateMachine(
    List(
      Turn, Coin, Turn, Coin, Turn
    )
  ).run(Machine(true,2,0)))