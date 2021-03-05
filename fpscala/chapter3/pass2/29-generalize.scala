sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


val a = Branch(Leaf(3),Branch(Leaf(5), Branch(Leaf(2), Leaf(1))))



def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
  case Leaf(x) => f(x)
  case Branch(l,r) => g(fold(l)(f)(g),fold(r)(f)(g))
}

def count[A](t: Tree[A]): Int =
  fold(t)((x:A)=>1)((l:Int,r:Int)=>l+r+1)

println(count(a))

def maximum(t: Tree[Int]): Int =
  fold(t)(x=>x)((x,y)=> x max y)

println(maximum(a))

def depth(t: Tree[Int]): Int =
  fold(t)(x=>1)((x,y)=> (x max y)+1)

println(depth(a))

def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
  fold(t)(x=>Leaf(f(x)):Tree[B])(Branch(_,_))

println(map(a)(x=>x * x))