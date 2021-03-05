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

def constant[A](a: A): Stream[A] =
  Stream.cons(a, constant(a))

def from(n: Int): Stream[Int] =
  Stream.cons(n, from(n + 1))

def fibs(nm: (Int, Int)): Stream[Int] = {
  Stream.cons(nm._1, fibs(nm._2, nm._1 + nm._2))
}

println(
  fibs(0,1)
    .take(6)
    .toList
)