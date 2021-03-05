import Stream._

sealed trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h()::t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def take(n: Int): Stream[A] =
    unfold((this, n)){
      case (Cons(h, t), i) if i > 0 =>  Some( h(), (t(), i - 1) )
      case _ => None
    }

  def drop(n: Int): Stream[A] =
    if (n == 0) this
    else this match {
      case Cons(_, t) => t().drop(n - 1)
      case _ => empty
    }

  def map[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def append[B >: A](z: => Stream[B]): Stream[B] =
    foldRight(z)((a, b) => cons(a, b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((h, t) => if (f(h)) cons(h, t) else t)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => f(h) append t)

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h, t) => if (p(h())) Some((h(), t())) else None
      case _ => None
    }

  def zipWith[B >: A](as2: Stream[B])(f: (B, B) => B): Stream[B] =
    unfold((this, as2)){
      case (Cons(s1, h1), Cons(s2, h2)) => Some(  (f(s1(), s2()), (h1(), h2())   ) )
      case _ => None
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())),(t1(), t2()))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())),(Empty, t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None),(t1(), Empty))
      case _ => None
    }

  def startsWith[B >: A](s2: Stream[B]): Boolean =
    zipAll(s2) takeWhile (_._2.isDefined) forAll {
      case (l, r) => l == r
    }

  def tails: Stream[Stream[A]] =
    unfold(this){
      case Empty => None
      case s => Some(s, s drop 1)
    } append Stream(empty)

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons( () => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

def fibs: Stream[Int] =
  unfold((0, 1)){ case (l, r) => Some((l, (l + r, l))) }

def from(n: Int): Stream[Int] =
  unfold(n)(i => Some(i, i + 1))

def constant[A](a: A): Stream[A] =
  unfold(a)(_ => Some((a, a)))

val ones: Stream[Int] =
  unfold(1)(_ => Some((1, 1)))



println(from(3).tails.take(1).toList.head.take(4).toList)
println(from(3).tails.take(2).toList(1).take(3).toList)
println(from(3).tails.take(3).toList(2).take(2).toList)
println(from(3).tails.take(4).toList(3).take(1).toList)
println(Stream(3, 4, 5, 6).tails.take(4).toList(3).take(2).toList)

println(Stream(1, 2).tails.toList.head.take(2).toList)
println(Stream(1, 2).tails.toList(1).take(1).toList)
 println(Stream(1, 2).tails.toList(2).take(1).toList)

println(Stream(1, 2, 3, 5) hasSubsequence Stream(2, 3))
println(Stream(1, 2, 3, 5) hasSubsequence Stream(3, 3))
println(Stream(1, 2, 3, 5) hasSubsequence Stream(3, 4))



