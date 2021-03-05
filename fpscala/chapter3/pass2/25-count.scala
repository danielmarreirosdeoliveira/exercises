sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


val a = Branch(Leaf(3),Branch(Leaf(5), Leaf(8)))



def count[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l,r) => 1 + count(l) + count(r)
}

println(count(a))