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

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a,b) => if (f(a)) Cons(() => a, () => b) else b)

  def take(n: Int): Stream[A] = unfold((this,n))({
    case (Cons(h, t),nn) => if (nn>0) Some((h(),(t(),nn-1))) else None
    case (Empty,_) => None
  })

  def takeWhile(p: A => Boolean): Stream[A] = unfold(this)({
    case Cons(h, t) => if(p(h())) Some((h(),t())) else None
    case Empty => None
  })

  def foldLeft[B](z: => B)(f: (B, A) => B): B = this match {
    case Cons(h,t) => t().foldLeft(f(z,h()))(f)
    case Empty => z
  }
}
def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] =
  f(s) match {
    case Some((aa,ss)) => Stream.cons(aa, unfold(ss)(f))
    case None => Empty
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
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

def scanLeft[A,B](s: Stream[A], z:B)(f:(=>B,A)=>B): Stream[B] =
  s.foldLeft((z,Stream(z)))((s2,a)=> s2 match {
    case (p1,p2) =>
      val b2 = f(p1,a)
      (b2, Stream.cons(b2, p2))
  })._2

//println(Stream(1,2,3).foldLeft(0)(_ + _))
//println(scanLeft(Stream(1,2,3),0)((b,a)=> {println("A",a); a+b}).take(2).toList)
println(scanLeft(Stream(1,2,3),0)((b,a)=> {println("A",a); a+b}).toList)
