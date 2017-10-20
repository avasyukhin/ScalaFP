sealed trait Tree[+A]
case class Leaf[A] (value:A) extends Tree[A]
case class Branch[A] (left: Tree[A],right: Tree[A]) extends Tree[A]

object Tree{
  def size[A](t:Tree[A]): Int = t match{
    case Leaf(_) => 1
    case Branch(c1,c2) => size(c1)+size(c2)
  }
  def maximum(t:Tree[Int]):Int = t match{
    case Leaf(a) => a
    case Branch(t1,t2) => maximum(t1) max maximum(t2)
  }
  def depth[A](t:Tree[A]): Int = t match{
    case Leaf(_) =>0 
    case Branch(c1,c2) => 1+ (depth(c1) max depth (c2))
  }
} 
