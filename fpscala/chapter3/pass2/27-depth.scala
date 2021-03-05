sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


val a = Branch(Leaf(3),Branch(Leaf(5), Branch(Leaf(2), Leaf(1))))



def depth(t: Tree[Int]): Int = t match {
  case Leaf(x) => 1
  case Branch(l,r) => (depth(l) max depth(r)) + 1
}

println(depth(a))