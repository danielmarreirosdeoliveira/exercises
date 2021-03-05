def findFirst[A](as: Array[A], term: A): Int = {

  @annotation.tailrec
  def go(as:Array[A], i: Int): Int = {

    if (i == as.length) return -1
    if (as(i) == term) return i
    go(as, i+1)
  }

  go(as, 0)
}

def findLast[A](as: Array[A], f: A => Boolean): Int = {

  @annotation.tailrec
  def go(as:Array[A], i: Int): Int = {

    if (i < 0) return -1
    if (f(as(i))) return i
    go(as, i-1)
  }

  go(as, as.length-1)
}

println(findFirst[Int](Array[Int](1,2,3,3,3,4),3))
println(findLast[Int](Array[Int](1,3,4),a => a < 4))