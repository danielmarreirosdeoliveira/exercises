sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


val a = Branch(Leaf(3),Branch(Leaf(5), Branch(Leaf(2), Leaf(1))))



def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
  case Leaf(x) => Leaf(f(x))
  case Branch(l,r) => Branch(map(l)(f), map(r)(f))
}

println(map(a)(x => x * x))