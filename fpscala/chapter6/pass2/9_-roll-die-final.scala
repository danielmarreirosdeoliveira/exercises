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

def int(rng: RNG): (Int, RNG) = {

  val (i1, rng1) = rng.nextInt
  if (i1 < 0) int(rng1)
  else (i1,rng1)
}

def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => rng => (f(a),rng))

def nonNegativeLessThan(n: Int): Rand[Int] =
  flatMap(int)(a => rng => {
    val mod = a % n
    if (a + (n-1) - mod >= 0) (mod, rng)
    else int(rng)
  })

def rollDie = map(nonNegativeLessThan(6))(_ + 1)

def go[A](n: Int, f: Rand[A]): Unit = {

  def _go(n: Int,r: RNG): Unit = {
    if (n > 0) {
      val (x, rn) = f(r)
      println(x)
      _go(n - 1, rn)
    }
  }
  _go(n,SimpleRNG(5))
}

println(go(25,rollDie))