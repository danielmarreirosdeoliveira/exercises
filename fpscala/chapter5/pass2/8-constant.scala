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

val ones: Stream[Int] = Stream.cons(1, ones)

println(
  ones.take(5)
    .filter(x => {println( "here",x); x < 8})
    .toList
)

def constant[A](a: A): Stream[A] =
  Stream.cons(a, constant(a))

println(
  constant("hey")
    .take(5)
    .toList
)