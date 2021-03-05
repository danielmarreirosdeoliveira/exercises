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



def modify[S](f: S => S): State[S, Unit] =
  State[S,S](s => (s,s))
    .flatMap((sAsValue: S) => State(_ => ((), f(sAsValue))))



def double = (x: Int) => x * 2

val s2 = modify(double).run(3)
val s3 = modify(double).run(s2._2)

println(s2)
println(s3)



val s4 = State((x:Int)=>{println(x);(x.toString+x.toString,x+1)})
  .flatMap((g:String)=>State((x:Int)=>{println(x);(g+g,x+1)}))
println(s4.run(3))