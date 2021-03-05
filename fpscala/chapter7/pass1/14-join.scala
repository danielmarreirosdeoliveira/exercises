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

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))


  // `````````

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    s => f(run(s)(a).get)(s)

  def join[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(para => para)

  def flatMapViaJoin[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    s => join[B](Par.unit(f(run(s)(a).get())))(s)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMapViaJoin(cond)(value => if (value) t else f)
}

println(
  Par.join[String](
    Par.unit(Par.unit("Hey"))
  )(es).get()
)
println(
  Par.choice[String](
    Par.unit(false)
  )(
    Par.unit("A"),
    Par.unit("B")
  )(es).get()
)