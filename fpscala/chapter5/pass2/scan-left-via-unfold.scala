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

def scanLeftViaUnfold[A,B](s: Stream[A], z:B)(f:(=>B,A)=>B): Stream[B] =
  unfold((s,z))({
    case (Cons(h,t),zz) => Some(    f(zz,h()),   (t(), f(zz,h()))   )
    case _ => None
  })



//println(scanLeftViaUnfold(Stream(1,2,3),0)((b,a)=> {println("A",a); a+b}).take(2))
println(scanLeftViaUnfold(Stream(1,4,7),0)((b,a)=> {println("A",a); a+b}).filter(x => {println("x",x); x > 0}).take(2))
