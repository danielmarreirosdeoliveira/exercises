sealed trait Stream[+A] {

  def toList: List[A] = {

    def go(s: Stream[A]): List[A] = s match {
      case Empty => Nil:List[A]
      case Cons(h,t) => h() :: go(t())
    }
    go(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def headOption: Option[A] =
    foldRight(None:Option[A])((a,b)=> a match {
      case x => Some(x)
    })

  def take(n: Int): Stream[A] = {

    def go(s: Stream[A], i: Int): Stream[A] = {
      s match {
        case Empty => Empty
        case Cons(h, t) => if (i < n) Cons(h, () => go(t(), i + 1)) else Empty
      }
    }
    go(this, 0)
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b)=> if (p(a)) Cons(() => a, () => b) else Stream.empty)

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a,b) => Stream.cons(f(a),b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a,b) => if (f(a)) Cons(() => a, () => b) else b)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a,b) => f(a))

  def append[AA >: A](e: => Stream[AA]): Stream[AA] =
    foldRight(e)((a,b)=>Stream.cons(a,b))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}

def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] =
  f(s) match {
    case Some((aa,ss)) => Stream.cons(aa, unfold(ss)(f))
    case None => Empty
  }
def ones(): Stream[Int] =
  unfold(1)(s => Some(1,1))
def constant[A](a: A): Stream[A] =
  unfold(a)(s => Some(a,a))
def from(n: Int): Stream[Int] =
  unfold(n)(s => Some(s,s+1))
def fibs(nm: (Int, Int)): Stream[Int] =
  unfold(nm)(s => Some(s._1,(s._2,s._1+s._2)))

println(ones().take(6).toList)
println(constant(3).take(6).toList)
println(from(7).take(6).toList)
println(fibs((0,1)).take(6).toList)