println("hello")

import java.util.concurrent._


val executorService: ExecutorService =
  new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS,
    new LinkedBlockingQueue[Runnable]())


class MyCallable extends Callable[String] {

  override def call(): String = "Back, I am!"
}

val fut = executorService.submit(new MyCallable())
println(fut.get())



