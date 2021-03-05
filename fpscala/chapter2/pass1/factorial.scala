object Factorial {

  def of(n: Int): Unit = {

    @annotation.tailrec
    def go(acc: Int, n: Int): Int = {
      if (n == 0) return acc;
      else go(acc*n,n-1);
    }

    println(":"+go(1,n))
  }
}