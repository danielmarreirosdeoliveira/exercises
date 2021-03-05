
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

@annotation.tailrec
def _thousand(rng: RNG): (Int,RNG) = {
  val (value, rn) = rng.next
  if (value < 0) {
    _thousand(rn)
  } else if (value > 999 ) { // with changed method this causes stackoverflow for low values like 999, with 999999999 it works for example
    _thousand(rn)
  } else {
    (value,rn)
  }
}

def thousand: Rand[Int] = // lift it instead of changing _thousand, see comment above
  rng => _thousand(rng)

def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a,rng2) = s(rng)
    (f(a), rng2)
  }

def doubleViaMap: Rand[Double] =
  map(thousand)((value: Int) => {println(value); 1.0 / makePlain(value)})

def map2[A,B,C](ra:Rand[A],rb:Rand[B])(f:(A,B)=>C):Rand[C] =
  rng => {
    val (a,rng1) = ra(rng)
    val (b,rng2) = rb(rng1)
    (f(a,b), rng2)
  }

def both[A,B](ra:Rand[A],rb:Rand[B]): Rand[(A,B)] =
  map2(ra, rb)((_,_))

def randDoubleInt: Rand[(Double,Int)] =
  both(doubleViaMap,thousand)

def randIntDouble: Rand[(Int,Double)] =
  both(thousand,doubleViaMap)

val (d1,r1) = map2(doubleViaMap, thousand)((_,_))(SimpleRNG(42))
val (d2,r2) = both(doubleViaMap, thousand)(r1)
val (d3,r3) = randIntDouble(r2)
val (d4,r4) = randDoubleInt(r3)

