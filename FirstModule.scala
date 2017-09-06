object FirstModule {
  def abs(n: Int): Int = 
    if (n < 0) -n
    else n

  private def formatAbs (x: Int) = {
    val msg = "Абсолютное значение %d это %d"
    msg.format(x, abs(x))
  }

  def read(): Unit ={
    val a = readInt()
    println(formatAbs(a))
  }
}
