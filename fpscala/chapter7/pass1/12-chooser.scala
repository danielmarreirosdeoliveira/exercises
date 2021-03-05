import java.util.concurrent._

val es: ExecutorService =
  new ThreadPoolExecutor(2, 2, 0L, TimeUnit.MILLISECONDS,
    new LinkedBlockingQueue[Runnable]())


type Par[A] = ExecutorService => Future[A]


object Par {

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {

    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map2[A,B,C](a: Par[A], b:Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))


  // ''''''''''
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val got: Int = run(es)(n).get

      def getEl(list: List[Par[A]])(n: Int): Par[A] = list match {
        case Nil => null
        case h::t =>
          if (n == got) h else getEl(t)(n+1)

      }

      run(es)(getEl(choices)(0))
    }

  def choiceMapNaive[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    choiceN(
      es => {
        val got = run(es)(key).get
        def i(list: List[K], n: Int): Int = list match {
          case Nil => 0
          case h::t => if (n == got) n else i(t, n+1)
        }
        UnitFuture(i(choices.keys.toList, 0))
      }
    )(choices.values.toList)

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
      s => choices(run(s)(pa).get)(s)


  def choiceMap[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    chooser(key)(got => choices.getOrElse(got, null))
}


println(
  Par.choiceMap[String, String](
    Par.unit("A")
  )(
      Map[String,Par[String]]("A" -> Par.unit("AA"), "B" -> Par.unit("BB"))
    )(es).get()
)


