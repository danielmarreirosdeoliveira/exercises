case class State[S](run: S => S) {



  def map(g: S => S): State[S] = State(s => {
    g(this.run(s))
  })

  def flatMap(g: S => State[S]): State[S] = State(s => {
    val b = this.run(s)
    g(b).run(b)
  })
}


val s = State((x:Int) => x * 2).map((x: Int) => x + 3)
println(s.run(3))

val s2 = State((x:Int) => x * 2)
  .flatMap((x: Int) => State(s => {println(x.toString+","+s.toString); x + 3 + s}))
println(s2.run(3))


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


def simulateMachine(inputs: List[Input]): State[Machine] = State((m: Machine) => {

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

  go(inputs, m)
})


println(simulateMachine(List(Coin,Turn,Coin,Turn)).run(Machine(true,1,2)))