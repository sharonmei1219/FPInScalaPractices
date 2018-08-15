package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{
	def sum(ints: List[Int]): Int = ints match{
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(x, xs) => x * product(xs) 
	}

	def tail[A](as: List[A]): List[A] = as match{
		case Nil => Nil
		case Cons(x, xs) => xs
	}

	def drop[A](as: List[A], n: Int): List[A] = as match{
		case Nil => Nil
		case Cons(x, xs) => {if (n == 0) as
							 else drop(xs, n-1)}
	}

	def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
		case Nil => Nil
		case Cons(x, xs) => {if (f(x)) dropWhile(xs, f)
							 else as} 
	}

	def foldRight[A, B](l: List[A], r: B)(f: (A, B) => B): B = l match {
		case Nil => r
		case Cons(x, xs) => f(x, foldRight(xs, r)(f)) 
	}

	def foldLeft[A, B](l: List[A], r: B)(f: (B, A) => B): B = l match {
		case Nil => r
		case Cons(x, Nil) => f(r, x)
		case Cons(x, xs) => foldLeft(xs, f(r, x))(f)
	}

	def length[A](l: List[A]): Int = {
		// foldRight(l, 0)((b: A, a: Int) => a + 1)
		foldLeft(l, 0)((a:Int, b:A) => a+1)
	}

	def sumLeftFold(l: List[Int]): Int = foldLeft(l, 0)((a:Int, b: Int)=>a+b)
	def productFoldLeft(l: List[Double]):Double = foldLeft(l, 1.0)((_*_)) 
	def reverse[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil)((nl: List[A], nx: A)=>Cons(nx, nl))

	def inc(a: Int): Int = a + 1

	def apply[A](as: A*): List[A] = 
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
}

