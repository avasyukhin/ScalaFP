sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head:A, tail:List[A]) extends List[A]


object List{
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    //case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x* product(xs)
  }
  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  def tail[A] (as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }
  def init[A] (as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x,Nil) => Nil  
    case Cons(x,xs) => Cons(x,init(xs))
  }
  def setHead[A] (as: List[A],a:A): List[A] = as match {
    case Nil => Nil
    case Cons(x,xs) => Cons(a,xs)
  }
  def drop[A] (as: List[A], n: Int): List[A] = (as,n) match {
    case (Nil,_) => Nil
    case (l,0) => l
    case (Cons(x,xs),k) => drop(xs,(k-1))
  }
  /*def dropWhile[A] (as: List[A], f: A=>Boolean): List[A] = as match {
    case Cons(h,t) if f(h) => dropWhile(t,f) 
    case _=> as 
  }*/
  def dropWhile[A] (as: List[A]) (f: A=>Boolean): List[A] = as match {
    case Cons(h,t)  if f(h) => dropWhile(t)(f) 
    case _=> as 
  }
  def append[A] (a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h,append(t, a2)) 
  }
  def appendByFold[A](a1: List[A],a2: List[A]):List[A]=
    rightFromLeft(a1,a2) (Cons(_,_))
  def foldRight[A,B](as: List[A],z:B) (f:(A, B)=>B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight (xs, z)(f))
  }
  def sum2(ns: List[Int])=
    foldRight(ns,0) ((x,y)=>x+y)
  def increment(ns: List[Int])=
    rightFromLeft(ns,Nil:List[Int]) ((a,b)=>Cons(a+1,b))
  def doubleToString(ns: List[Double])=
    rightFromLeft(ns,Nil:List[String]) ((a,b)=>Cons(a.toString,b))
  def product2(ns: List[Double])=
    foldRight(ns,1.0) (_*_)
  def length[A](as: List[A]):Int=
    foldRight(as,0) ((_,i)=>i+1)
  def leftFromRight[A,B](as: List[A],z:B)(f:(B,A)=>B):B=
    foldRight(as,(b:B)=>b) ((a,g)=>b=>g(f(b,a))) (z)
  def foldLeft[A,B](as:List[A],z:B)(f:(B,A)=>B):B={
    @annotation.tailrec
    def loop (l:List[A],b:B): B = l match{
      case Nil => b
      case Cons (x,xs) => loop(xs,f(b,x))
    }      
    loop(as,z)
  }
  def rightFromLeft[A,B](as:List[A],z:B) (f:(A,B)=>B): B=
    foldLeft(as,(b:B)=>b) ((g,a)=>b=>g(f(a,b))) (z)
  def reverse[A](as: List[A]):List[A]=
    foldLeft(as,Nil:List[A]) ((xs,x)=>Cons(x,xs))
  def appendAll[A] (ss: List[List[A]]):List[A]=
   foldLeft(ss,Nil:List[A]) (appendByFold)
  def map[A,B] (as: List[A])(f:A=>B):List[B]=
    rightFromLeft(as,Nil:List[B])((a,bs)=>Cons(f(a),bs))
  def filter[A] (as: List[A])(f:A=>Boolean):List[A]=
    flatMap(as)(a=>if (f(a)) Cons(a,Nil:List[A]) else Nil:List[A]) 
  def flatMap[A,B] (as: List[A])(f:A=>List[B]):List[B]=
    rightFromLeft(as,Nil:List[B])((a,bs)=>appendByFold(f(a),bs))
  @annotation.tailrec
  def hasSubsequence[A](sup:List[A],sub: List[A]):Boolean = sup match {
    case Nil => false
    case Cons(p,ps) => if (startsWith(Cons(p,ps),sub)) true else hasSubsequence(ps,sub)
  }
  @annotation.tailrec
  def startsWith[A](sup:List[A],sub: List[A]):Boolean = (sup,sub) match{
    case (_,Nil) => true
    case (Cons(p,ps),Cons(b,bs)) if (p==b) => startsWith(ps,bs)
    case _ => false
  }
}
