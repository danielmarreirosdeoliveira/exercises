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

  def map[B](f: A => B): Stream[B] = unfold(this)({
    case Cons(h,t) => Some((f(h()), t()))
    case Empty => None
  })

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b )

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b )

  def take(n: Int): Stream[A] = unfold((this,n))({
    case (Cons(h, t),nn) => if (nn>0) Some((h(),(t(),nn-1))) else None
    case (Empty,_) => None
  })

  def takeWhile(p: A => Boolean): Stream[A] = unfold(this)({
    case Cons(h, t) => if(p(h())) Some((h(),t())) else None
    case Empty => None
  })
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

def zipWith[A,B](as1: Stream[A], as2: Stream[A])(f: (A,A) => B): Stream[B] = {
  unfold((as1,as2))({
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(),h2()),(t1(),t2())))
    case (_, _) => None
  })
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

def startsWith[A](s1: Stream[A], s2: Stream[A]): Boolean =
  zipWithAll(s1, s2)((oa, ob) => oa match {
    case Some(oai) => ob match {
      case Some(obi) => Some(oai == obi)
      case None => Some(true)
    }
    case None => None
  }).forAll {
    case Some(x) => x
    case None => false
  }

def tails[A](s: Stream[A]): Stream[Stream[A]] =
  unfold(s) (ss => ss match {
    case Cons(h, t) => Some((ss, t()))
    case _ => None
  })

def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
  tails(s1).exists(sl => startsWith(sl, s2))

println(tails(Stream(1,2,3)).toList.foldRight(Nil:List[List[Int]])((a,b) => a.toList::b))
println(tails(Stream(1,2,3)).foldRight(Nil:List[List[Int]])((a,b) => a.toList::b))

println(hasSubsequence(Stream(1,2,3),Stream(1,2,3)))
println(hasSubsequence(Stream(1,2,3),Stream(1,2)))
println(hasSubsequence(Stream(1,2,3),Stream(2,2)))
println(hasSubsequence(Stream(1,2),Stream(1,2,3)))
