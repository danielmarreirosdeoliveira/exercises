


def thunk(a: => Unit, b: Boolean) =
  if (b) a

def nonThunk(a: () => Unit, b: Boolean) =
  if (b) a()


thunk(println("3"),b = true)
thunk(println("4"),b = false)
nonThunk(() => println("3"),b = true)
nonThunk(() => println("4"),b = false)


def laz(a: => String) = {
  println("right")
  lazy val j = a
  j+j
}

println(laz({println(", now"); "!"}))


def laz2(a: () => String) = {
  println("right")
  lazy val j = a
  j()+j()
}

println(laz2(() => {println(", now"); "!"}))





