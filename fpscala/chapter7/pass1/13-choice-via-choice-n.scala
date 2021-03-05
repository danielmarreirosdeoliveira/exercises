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

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    s => choices(run(s)(pa).get)(s)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(value => if (value) t else f)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(value => {

      def getEl(list: List[Par[A]])(n: Int): Par[A] = list match {
        case Nil => null
        case h::t =>
          if (n == value) h else getEl(t)(n+1)
      }
      getEl(choices)(0)
    })

}

println(
  Par.choice[String](
    Par.unit(true)
  )(
    Par.unit("A"),
    Par.unit("B")
  )(es).get()
)

println(
  Par.choiceN(Par.unit(1))(List(
    Par.unit("A"),
    Par.unit("B"))
  )(es).get()
)
