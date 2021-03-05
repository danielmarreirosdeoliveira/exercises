
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

def positiveNr: Rand[Int] = // lift it instead of changing _thousand, see comment above
  rng => _positiveNr(rng)


def flatMap[A,B](s: Rand[A])(f: A => Rand[B]): Rand[B] =
  rng => {
    val (a,rng2) = s(rng)
    f(a)(rng2)
  }

def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a,rng2) = s(rng)
    (f(a), rng2)
  }

def map2[A,B,C](ra:Rand[A],rb:Rand[B])(f:(A,B)=>C):Rand[C] =
  rng => {
    val (a,rng1) = ra(rng)
    val (b,rng2) = rb(rng1)
    (f(a,b), rng2)
  }

def mapViaFlatMap[A,B](s:Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => { rng => (f(a),rng)})

def doubleViaMapViaFlatMap: Rand[Double] =
  mapViaFlatMap(thousand)((value: Int) => {println(value); 1.0 / makePlain(value)})

doubleViaMapViaFlatMap(SimpleRNG(42))

def map2ViaFlatMap[A,B,C](ra:Rand[A],rb:Rand[B])(f:(A,B)=>C):Rand[C] =
  flatMap(ra)(a => flatMap(rb)(b => rb => (f(a,b),rb) ))

def bothViaMap2ViaFlatMap[A,B](ra:Rand[A],rb:Rand[B]): Rand[(A,B)] =
  map2ViaFlatMap(ra, rb)((_,_))

val (v1,r1) = bothViaMap2ViaFlatMap(thousand,doubleViaMapViaFlatMap)(SimpleRNG(42))
val (v2,r2) = bothViaMap2ViaFlatMap(thousand,doubleViaMapViaFlatMap)(r1)