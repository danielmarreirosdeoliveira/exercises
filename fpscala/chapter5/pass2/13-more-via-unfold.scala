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

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a,b) => if (f(a)) Cons(() => a, () => b) else b)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a,b) => f(a))

  def append[AA >: A](e: => Stream[AA]): Stream[AA] =
    foldRight(e)((a,b)=>Stream.cons(a,b))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def map[B](f: A => B): Stream[B] = unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case Empty => None
    }

  def take(n: Int): Stream[A] = unfold((this,n)) {
      case (Cons(h, t),nn) => if (nn>0) Some((h(),(t(),nn-1))) else None
      case (Empty,_) => None
    }

  def takeWhile(p: A => Boolean): Stream[A] = unfold(this) {
      case Cons(h, t) => if(p(h())) Some((h(),t())) else None
      case Empty => None
    }
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

def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] =
  f(s) match {
    case Some((aa,ss)) => Stream.cons(aa, unfold(ss)(f))
    case None => Empty
  }

def zipWith[A](as1: Stream[A], as2: Stream[A])(f: (A,A) => A): Stream[A] = {
  unfold((as1,as2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case (_, _) => None
  }
}

def zipWithAll[A, B, C](s1: Stream[A], s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
  unfold((s1, s2)) {
    case (Empty, Empty) => None
    case (Cons(h, t), Empty) => Some(
      (
        f(Some(h()), Option.empty[B]),
        (t(), Stream.empty[B])
      )
    )
    case (Empty, Cons(h, t)) => Some(
      (
        f(Option.empty[A], Some(h())),
        (Stream.empty[A],t())
      )
    )
    case (Cons(h1, t1), Cons(h2, t2)) => Some(
      (f(Some(h1()),Some(h2())), (t1(),t2()))
    )
  }


println(zipWithAll(Stream(1,2,3),Stream(1))((_,_)).take(4).toList)

println(Stream(1,2,3,4,5).map(_ * 2).take(2)
  .filter(x => x < 4).toList)

println(Stream(1,2,3,4,5).map(_ * 2).takeWhile(_ < 5)
  .filter(x => x < 3).toList)

println(zipWith(Stream(1,2,3),Stream(7,2,1))((a,b)=>a+b)
  .take(2).filter(_ > 5).toList)

//println(zipWith(Stream(1,2,3),Stream(7,2,1,1))((a,b)=>a+b).toList)
//println(zipWith(Stream(1,2,3,3),Stream(7,2,1,1))((a,b)=>a+b).toList)
//println(zipWith(Stream(1,2,3,3),Stream(7,2))((a,b)=>a+b).toList)
//println(zipWith(Stream(),Stream(7,2,1,1))((a,b)=>a+b).toList)