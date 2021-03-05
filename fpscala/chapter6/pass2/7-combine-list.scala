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

type Rand[+A] = RNG => (A, RNG)

def pos(rng: RNG): (Int, RNG) = {

  val (i1, rng1) = rng.nextInt
  if (i1 < 0) pos(rng1)
  else (i1,rng1)
}

def neg(rng: RNG): (Int, RNG) = {

  val (i1, rng1) = rng.nextInt
  if (i1 >= 0) neg(rng1)
  else (i1,rng1)
}


def transitionViaFoldRight[A](a: List[Rand[A]], rng: RNG): (List[A],RNG) =
  a.foldRight((Nil:List[A], rng))((rand, b) => {
    val (v, r2) = rand(b._2)
    (v::b._1, r2)
  })

def transitionViaFoldLeft[A](a: List[Rand[A]], rng: RNG): (List[A],RNG) =
  a.foldLeft((Nil:List[A], rng))((b, rand) => {
    val (v, r2) = rand(b._2)
    (v::b._1, r2)
  })

def ints(n: Int): (List[Int], RNG) =
  transitionViaFoldRight(List.fill(n)(pos _), SimpleRNG(42))

//println(transitionViaFoldRight(List(pos _, neg _, pos _, neg _),SimpleRNG(42)))
//println(transitionViaFoldLeft(List(neg _, pos _, neg _, pos _),SimpleRNG(42)))

println(ints(4))


