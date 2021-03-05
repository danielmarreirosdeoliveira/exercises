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

def double: Rand[Double] =
  map[Int,Double](int)(i => i % 1000 / 1000.0)

def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

def map2[A, B, C](sa: Rand[A], sb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng => {
    val (a, rng2) = sa(rng)
    val (b, rng3) = sb(rng2)
    (f(a,b), rng3)
  }


def both[A,B](sa: Rand[A], sb: Rand[B]): Rand[(A,B)] =
  map2(sa,sb)((_, _))

val randIntDouble: Rand[(Int, Double)] =
  both(int, double)



def go[A](n: Int, f: Rand[A]): Unit = {

  def _go(n: Int,r: RNG): Unit = {
    if (n > 0) {
      val (x, rn) = f(r)
      println(x)
      _go(n - 1, rn)
    }
  }
  _go(n,SimpleRNG(42))
}

go(5,randIntDouble)
