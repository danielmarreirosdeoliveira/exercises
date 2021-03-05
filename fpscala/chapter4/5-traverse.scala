sealed trait Option[+A] {

  def getOrElse[B >: A](default: => B): B = this match { // pm
    case None => default
    case Some(x) => x
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(a => Some(a)).getOrElse(ob)

  def map[B](f: A => B): Option[B] = this match { // pm
    case None => None
    case Some(x) => Some(f(x))
  }
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a.foldRight(Some(Nil:List[B]):Option[List[B]])((a: A, b) => f(a).flatMap(a => b.flatMap(
    lb => Some(a::lb)
  )))

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None }

def compose[A,B,C](f: B => C, g: A => B): A => C =
  a => f(g(a))

def flattenEmtpy[A](a: Option[List[A]]): Option[List[A]] =
  a.flatMap(lista => if (lista.isEmpty) None else Some(lista))

def sequence_[A](a: List[Option[A]]): Option[List[A]] =
  traverse(a)(a => a.flatMap(ia => Some(ia)))

def sequence[A] =
  compose(flattenEmtpy[A], sequence_[A])

println(traverse(List(Some(1),Some(2),Some(3)))(a => a.flatMap(i => if (i==1) None else Some(i))))
println(traverse(List(Some(2),Some(3)))(a => a.flatMap(i => if (i==1) None else Some(i))))

println(sequence(List(Some(1),Some(2),Some(3))))
println(sequence(List(Some(1),None,Some(3))))
println(sequence(List()))