def isSorted[A](as:Array[A], f:(A,A) => Boolean): Boolean = {

  @annotation.tailrec
  def go(as: Array[A], f:(A,A) => Boolean, i: Int): Boolean =
    if (i == as.length) true
    else if (i-1 < 0 || f(as(i-1),as(i))) go(as, f, i + 1)
    else false

  go(as, f, 0)
}


println(isSorted(Array(),(a:Int,b:Int)=>a > b))
println(isSorted(Array(1),(a:Int,b:Int)=>a > b))
println(isSorted(Array(1,1),(a:Int,b:Int)=>a > b))
println(isSorted(Array(1,2),(a:Int,b:Int)=>a < b))


def lessThan(a:Int,b:Int): Boolean = a < b
println(isSorted(Array(1,2),lessThan))

def lessThan2 = new Function2[Int,Int,Boolean] {
  def apply(a:Int,b:Int):Boolean = a<b
}
println(isSorted(Array(1,1),lessThan2))