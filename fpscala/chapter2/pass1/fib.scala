object Fib {

  def of(v: Int): Int = {

    @annotation.tailrec
    def go(prev: Int, current: Int, n: Int): Int = {

      if (n == v) return current;
      go(current, current+prev,n+1)
    }

    if (v == 1) return 1;
    if (v == 0) return 0;
    go(0, 1, 1)
  }
}


/*

   von 2

   2. 1:   go(0,1)

   0,0,1 = 1






 */