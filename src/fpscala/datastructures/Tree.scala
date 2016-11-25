package fpscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf => 1
    case Branch(l,r) => 1+ size(l)+size(r)
  }
  def maximum(tree: Tree[Int]): Int = tree match{
    case Leaf(value) => value
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]):Int = tree match {
      case Leaf => 0
      case Branch(l,r) => 1+ (depth(l) max depth(r))
  }

  def map[A,B](l:Tree[A])(f:A=>B):Tree[B]=l match{
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
  }
  def fold[A,B](tree:Tree[A])(f:(A)=>B)(g:(B,B)=>B): B=tree match{
    case Leaf(v) => f(v)
    case Branch(l,r) => g(fold(l)(f)(g),fold(r)(f)(g))
  }

  def size2[A](tree: Tree[A]): Int = fold(tree)(1)((b1,b2) => 1+b1+b2)
  def maximum2(tree: Tree[Int]): Int = fold(tree)(x=>x)((b1,b2)=> b1 max b2)
  def depth2[A](tree: Tree[A]):Int = fold(tree)(0)((b1,b2)=> 1+ (b1 max b2))
  def map2[A,B](l:Tree[A])(f:A=>B):Tree[B]= fold(tree)((a)=>Leaf(f(a)): Tree[B])((b1,b2)=>Branch(b1,b2))

}