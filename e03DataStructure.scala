sealed trait List[+A]
case object Nil extends List[Nothing] 
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{

  def foldLeft[A, B](as: List[A], z: B)(f:(A, B)=>B):B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(x, z))(f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B):B = as match{
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def reverse[A](ints: List[A]) = 
    foldLeft(ints, Nil:List[A])((x:A, rPre:List[A]) => Cons(x, rPre))

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B):B = 
    foldLeft(reverse(as), z)(f)

  def map[A, B](as: List[A])(f:A => B):List[B] = 
    foldRightViaFoldLeft(as, Nil:List[B])((d, l) => Cons(f(d), l))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(as, Nil:List[A])((d, l) => if (f(d)) Cons(d, l) else l)

  def length(ints: List[Int]) =
    foldLeft(ints, 0)((a, b) => b + 1)

  def inc(as: List[Int]) =
    map(as)(_+1)

  def toString(as: List[Double]) = 
    map(as)(_.toString)

  def sum(ints: List[Int]) =
    foldLeft(ints, 0)(_+_)

  def product(ints: List[Int]) =
    foldLeft(ints, 1)(_*_)

  def tail(ints: List[Int]): List[Int] = ints match{
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead(x: Int, ints:List[Int]):List[Int] = Cons(x, tail(ints))

  def drop[A](l: List[A], n: Int): List[A] = l match{
    case Cons(x, xs) if (n != 0) => drop(xs, n-1)
    case _ => l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match{
    case Cons(x, xs) if(f(x)) => dropWhile(xs, f)
    case _ => l
  }

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))

}

println(List.filter(List(1,2, 3, 4))(_%2 == 0))
println(List.inc(List(1, 2, 3, 4)))
println(List.length(List(1, 2)))
println(List.reverse(List(1, 2)))

val x = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}
println(x)