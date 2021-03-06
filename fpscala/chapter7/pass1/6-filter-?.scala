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


  def sequence[A](ps:List[Par[A]]): Par[List[A]] = {

    es => UnitFuture(ps.foldLeft(Nil:List[A])((b,para)=> para(es).get()::b ))
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    unit(as)
  }
}


println(
  Par.parFilter(List(1,2,4))(x => x > 3)(es).get()
)



