
trait RNG  {
  def next: (Int,RNG)
}

type Rand[+A] = RNG => (A, RNG)

case class SimpleRNG(seed: Long) extends RNG {

  override def next: (Int, SimpleRNG) = {

    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

def makePlain(n: Double): Double = {
  if ((n / 10) >= 1.0) {
    makePlain(n / 10)
  } else {
    n
  }
}

def _positiveNr(rng:RNG): (Int,RNG) = {
  val (value, rn) = rng.next
  if (value <= Int.MinValue) {
    _positiveNr(rn)
  } else if (value < 0 && value > Int.MinValue) {
    (-value, rn)
  } else if (value > Int.MaxValue) {
    _positiveNr(rn)
  } else {
    (value,rn)
  }
}

def positiveNr: Rand[Int] = // lift it instead of changing _thousand, see comment above
  rng => _positiveNr(rng)


def flatMap[A,B](s: Rand[A])(f: A => Rand[B]): Rand[B] =
  rng => {
    val (a,rng2) = s(rng)
    f(a)(rng2)
  }


def _nonNegativeLessThan(i:Int,n:Int): Rand[Int] = {
  rng => {
    val mod = i % n
    if (i + (n-1) - mod >= 0) (mod,rng) else _nonNegativeLessThan(i,n)(rng)
  }
}

def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a,rng2) = s(rng)
    (f(a), rng2)
  }

def nonNegativeLessThan(n:Int) =
  flatMap(positiveNr)((i:Int) => {
    _nonNegativeLessThan(i,n)})

def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

rollDie(SimpleRNG(11))._1

