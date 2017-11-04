object LazyStructures{
sealed trait Stream[+A]{
  def toList: List[A] = this match{
    case Empty => Nil
    case Cons(x,xs) => x()::(xs().toList)
  }

  def take(n:Int):Stream[A]=
    Stream.unfold((this,n)){
      case (Cons(h,t),1)=>Some((h(),(Stream.empty,0)))
      case (Cons(h,t),n) if n>1 => Some((h(), (t(),n-1)))
      case _ =>None
    } 
  /*this match{
    case Empty => Stream.empty
    case Cons(x,xs) if n>0 => Stream.cons(x(), xs() take(n-1))
    case Cons(x,xs) => Stream.cons(x(),Stream.empty) 
  }*/
  def foldRight[B] (z: => B)(f:(A, => B)=> B): B = this match{
    case Cons(h,t) => f(h(),t().foldRight(z)(f))
    case _ => z
  }
  def exists(p:A => Boolean):Boolean =
    foldRight(false)((a,b) => p(a) || b)
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)
  def takeWhile(p: A => Boolean):Stream[A]=
    Stream.unfold(this){
      case Cons(x,xs) if p(x()) => Some((x(),xs()))
      case _ => None      
    }
    //foldRight(Stream.empty:Stream[A])((a,b) => if (p(a)) Stream.cons(a,b) else b)
  def zipAll[B] (s2:Stream[B]):Stream[(Option[A],Option[B])]=
    Stream.unfold(this,s2){
      case (Cons(a,as),Cons(b,bs)) => Some((Some(a()),Some(b())),(as(),bs()))
      case (Cons(a,as),_) => Some(((Some(a()),None),(as(),Empty)))
      case (_, Cons(b,bs)) => Some(((None,Some(b())),(Empty,bs())))
      case _ => None
    }
  def headOption: Option[A] =
    foldRight(None:Option[A])((a,b)=> Some(a) orElse b )
  def map[B] (f: A => B): Stream[B] =
    //foldRight(Stream.empty : Stream[B])((a,b) => Stream.cons(f(a),b))
    Stream.unfold(this){case Empty => None; case Cons(x,xs)=>Some((f(x()),xs()))}
  def append[B>:A](xs: Stream[B]): Stream[B] =
    foldRight(xs)((a,b) => Stream.cons(a,b))
  def flatMap[B](f:A=>Stream[B]):Stream[B] =
    foldRight(Stream.empty: Stream[B])((a,b) => f(a) append b)
  def filter(f:A=>Boolean):Stream[A] =
    flatMap(a => if (f(a)) Stream(a) else Stream.empty: Stream[A])
  def startsWith[B>:A](s:Stream[B]):Boolean =
    zipAll(s) takeWhile (!_._2.isEmpty) forAll{
      case (h1,h2)=> h1==h2
    } 
  def tails:Stream[Stream[A]]=
    Stream.unfold(this){
      case Cons(x,xs)=>Some((xs(),xs()))
      case _ => None
    }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=> A, t: () => Stream[A]) extends Stream[A]

object Stream{
  def cons[A] (hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(()=>head, ()=>tail)
  }
  def empty[A]:Stream[A] = Empty

  def apply[A] (as: A*):Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail:_*))
  def constant[A] (a: A): Stream[A]=
    unfold(())(_=>Some((a,())))
 def fibs:Stream[Int] ={
   unfold((0,1))({case (n,m)=>Some(n,(m,n+m))})   
 } 
 def from(n:Int):Stream[Int] =
   unfold(n)(i=>Some(i,i+1))   
 def unfold[A,S] (z:S)(f:S=>Option[(A,S)]):Stream[A]= f(z) match{
   case None => empty
   case Some((a,s)) => cons(a, unfold(s)(f))
 }
 
}
}
