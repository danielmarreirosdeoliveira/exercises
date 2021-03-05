sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


val a = Branch(Leaf(3),Leaf(4))
println(a)


a match {
  case Branch(_,Leaf(4)) => println("Yes")
  case _ => println("no")
}