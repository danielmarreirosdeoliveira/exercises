def r(h: => ()=>Int): ()=>Int = {
  lazy val ha = h
  ha
}

def ha = () => {println("hier"); 4}

//println(r(ha))
println(r(ha)())