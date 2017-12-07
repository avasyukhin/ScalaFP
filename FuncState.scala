trait RNG{
  def nextInt: (Int,RNG)
}
case class SimpleRNG(seed:Long) extends RNG{
  def nextInt:(Int,RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n,nextRNG)
  }
  def nonNegativeInt:(Int,SimpleRNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    val nPos = n match {
      case Int.MinValue => Int.MaxValue
      case k if (k<0) => -1*k
      case k => k
      }
    (nPos,nextRNG)
    }
}
