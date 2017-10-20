sealed trait Option[+A]{
  def map[B] (f: A=>B):Option[B]= this match{
    case None => None
    case Some(a) => Some(f(b))
  }
  def flatMap[B](f:A=>Option[B]) = 
    if (this!=None) f(this.get) else None
  def getOrElse[B>:A](default: => B):B = this match{
    case None => default
    case Some(a) => a
  }
  def orElse[B>:A](ob: =>Option[B]):Option[B] = 
    if (this!=None) this else ob
  def filter(f:A=>Boolean):Option[A]=
    if (f(this.get)) this else None 
}
case class Some[+A] (get:A) extends Option[A]
case object None extends Option[Nothing]

