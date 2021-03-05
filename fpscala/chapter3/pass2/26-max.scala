sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


val a = Branch(Leaf(3),Branch(Leaf(5), Leaf(8)))



def max_(t: Tree[Int]): Int = t match {
  case Leaf(x) => x
  case Branch(l,r) => max_(l) max max_(r)
}

println(max_(a))