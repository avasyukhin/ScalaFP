trait RNG{
  def nextInt: (Int,RNG)
}
case class SimpleRNG(seed:Long) extends RNG{
type Rand[+A] = RNG => (A, RNG)
def map[A,B](s: Rand[A])(f: A => B): Rand[B]=
  rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }
  def unit[A](a:A):Rand[A]=
    rng => (a,rng)
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
  def double(rng: RNG):(Double,RNG) = {
    val (int,nextRNG) = rng.nextInt
    val d = int match{
      case k if ((k==Int.MinValue)||(k==Int.MaxValue)) => Int.MaxValue-1
      case k if (k<0) => -1*k
      case k => k
    } 
    (d.toDouble/Int.MaxValue,nextRNG)
  }
 def doubleFromMap:Rand[Double]=
   map(_.nextInt)(i => {val d = i match{
      case k if ((k==Int.MinValue)||(k==Int.MaxValue)) => Int.MaxValue-1
      case k if (k<0) => -1*k
      case k => k
   }
   d.toDouble/Int.MaxValue}
   )
 def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
   rng =>{
     val (a,rng1) = ra(rng)
     val (b,rng2) = rb(rng1)
     (f(a,b),rng2)
   }
 def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
   fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

 def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
   rng =>{
    val (a,rng1) = f(rng)
    g(a)(rng1)
   }
}
