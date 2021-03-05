

def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
  if (cond) onTrue() else onFalse()

def if3[A] (cond: Boolean, onTrue: => A, onFalse: => A): A =
  if (cond) onTrue else onFalse

// if2(2 == 2, println("yes"), println("no")), syntax
if2(2 == 2, () => println("yes"), () => println("no"))
if2(2 == 2, {() => println("yes")}, {() => println("no")})
if2(2 == 2, () => {println("yes")}, () => {println("no")})
if3(2 == 2, println("yes"), println("no"))
if3(2 == 2, {println("yes")}, {println("no")})


def maybeTwice(b: Boolean, i: => Int) = if (b) i + i else 0
println(maybeTwice(true, {println("hi"); 42}))

def maybeTwice2(b: Boolean, i: => Int) = {
  lazy val j = i
  if (b) j + j else 0
}
println(maybeTwice2(true, {println("hi2"); 42}))