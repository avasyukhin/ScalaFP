import scala.{Option => _, Either => _, _}
object ErrorHandling{
sealed trait Option[+A]{
  def map[B] (f: A=>B):Option[B]= this match{
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f:A=>Option[B]) :Option[B]= 
    map(f) getOrElse None
  def getOrElse[B>:A](default: => B):B = this match{
    case None => default
    case Some(a) => a
  }
  def orElse[B>:A](ob: =>Option[B]):Option[B] = 
    if (this!=None) this else ob
  def filter(f:A=>Boolean):Option[A]=
    if (map(f) getOrElse false) this else None  
}
case class Some[+A] (get:A) extends Option[A]
case object None extends Option[Nothing]

object Option{
  def map2[A,B,C](a:Option[A], b:Option[B]) (f:(A,B)=>C):Option[C]= (a,b) match{
    case (None,_)=>None
    case (_,None) => None
    case (Some(x),Some(y)) =>Some(f(x,y))
  }
  def sequence[A] (a:List[Option[A]]): Option[List[A]] = 
   traverse[Option[A],A](a)(o=>o)
  def traverse[A,B](a:List[A])(f:A=>Option[B]):Option[List[B]] =
   a.foldRight[Option[List[B]]](Some(Nil))((x,y)=>map2(f(x),y)(_::_))
}

sealed trait Either[+E,+A]{
  def map[B](f:A=>B):Either[E,B] = this match{
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }
  def orElse[EE>:E,B>:A](b: =>Either[EE,B]):Either[EE,B]=this match{
    case Left(e) => b
    case Right(a) => Right(a)
  }
  def flatMap[EE>:E,B](f: A=>Either[EE,B]):Either[EE,B]= this match{
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }
  def map2[EE>:E,B,C](b: Either[EE,B])(f:(A,B)=>C):Either[EE,C]=
    this flatMap (aa => b map (bb=>f(aa,bb)))
}
case class Left[+E](value:E) extends Either[E,Nothing]
case class Right[+A](value:A) extends Either[Nothing,A]

object Either{
  def sequence[E,A] (es:List[Either[E,A]]): Either[E,List[A]] = 
    es.foldRight[Either[E,List[A]]](Right(Nil))((x,y)=>x.map2(y)(_::_))
  def traverse[E,A,B](as:List[A])(f:A=>Either[E,B]):Either[E,List[B]] =
   as.foldRight[Either[E,List[B]]](Right(Nil))((x,y)=>f(x).map2(y)(_::_))
}
def variance(xs:Seq[Double]):Option[Double]={
  Some(xs) flatMap(xs => if (xs.length==0) None else Some(xs.map(x => math.pow(x-xs.sum/xs.length,2)).sum/xs.length)) 
}
}
