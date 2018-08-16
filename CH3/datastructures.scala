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


	def lengthByFoldRight[A](l: List[A]): Int = {
		foldRight(l, 0)((b: A, a: Int) => a + 1)
	}

	def lengthByFoldLeft[A](l: List[A]): Int = {
		foldLeft(l, 0)((a:Int, b:A) => a+1)
	}

	def sumLeftFold(l: List[Int]): Int = foldLeft(l, 0)((a:Int, b: Int)=>a+b)
	def productFoldLeft(l: List[Double]):Double = foldLeft(l, 1.0)((_*_)) 
	def reverse[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil)((nl: List[A], nx: A)=>Cons(nx, nl))


	// def foldRightByFoldLeft[A, B](l: List[A], r: B)(f:(A,B) => B): B = {

	// 	def funcGenerator (func: (B) => B, x: A) : (B) => B = {
	// 		(r:B) => f(x, func(r))
	// 	}

	// 	foldLeft(l, (r:B) => r)(funcGenerator)(r)
	// }

	def foldRightByFoldLeft[A, B](l: List[A], r: B)(f:(A,B) => B): B = {
		def G(func: B => B, x: A): B => B = {
			(r:B) => func(f(x, r))
		}

		foldLeft(l, (r:B) => r)(G)(r)
	}

	def foldLeftByFoldRight[A, B](l: List[A], r: B)(f: (B, A) => B): B = {
		def G(x: A, func: B => B): B => B = {
			(r: B) => f(func(r), x)
		}

		foldRight(l, (r:B) => r)(G)(r)
	}

	def appendByFoldLeft[A](l1: List[A], l2: List[A]): List[A] = {
		def G(func: List[A] => List[A], x: A): List[A] => List[A] = {
			(r: List[A]) => func(Cons(x, r))
		}

		foldLeft(l1, (l: List[A]) => l)(G)(l2)
	}

	def appendByFoldRight[A](l1: List[A], l2: List[A]): List[A] = {
		foldRight(l1, l2)((x: A, l: List[A]) => Cons(x, l))
	}

	def concatenates[A](lol: List[List[A]]) = {
		foldLeft(lol, List[A]())(appendByFoldLeft)
	}


	def map[A, B](l: List[A])(f: A=>B):List[B] = {
		foldRightByFoldLeft(l, List[B]())((x: A, r: List[B]) => Cons(f(x), r))
	}

	def filter[A](l: List[A])(f: A=>Boolean): List[A] = {
		foldRightByFoldLeft(l, List[A]())((x: A, r: List[A])=> {
			if (f(x)) Cons(x, r)
			else r
		})
	}

	def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
		foldRightByFoldLeft(l, List[B]())((x: A, r: List[B]) => appendByFoldLeft(f(x), r))
	}

	def filterByFlatMap[A](l: List[A])(f: A=>Boolean): List[A] = {
		flatMap(l)((x:A) => {
			if (f(x)) List(x)
			else Nil
		})
	}

	def merge[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C) : List[C] = (l1, l2) match {
		case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), merge(xs, ys)(f))
		case (_, _) => Nil 
	}

	def mergeTOC[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C) : List[C] = {
		def go(l1t: List[A], l2t: List[B], r: List[C] => List[C]): List[C] = (l1t, l2t) match {
			case (Cons(x, xs), Cons(y, ys)) => go(xs, ys, (l: List[C]) => r(Cons(f(x, y), l)))
			case (_, _) => r(Nil)
		}

		go(l1, l2, (r:List[C]) => r)
	}


	def tracePlus(a:Int, b:Int):Int = {
		print(a)
		print("+")
		println(b)
		println("=====================")
		a+b
	}

	def incList(l: List[Int]): List[Int] = {
		foldRightByFoldLeft(l, List[Int]())((x: Int, r: List[Int]) => Cons(x + 1, r))
	}

	def toStringList(l: List[Double]): List[String] = {
		foldRightByFoldLeft(l, List[String]())((x: Double, r: List[String]) => Cons(x.toString, r))
	}


	def inc(a: Int): Int = a + 1

	def apply[A](as: A*): List[A] = 
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
	def size[A](t: Tree[A]): Int = t match {
		case Leaf(x) =>  1
		case Branch(l, r) => size(l) + size(r) + 1
	}

	def maximum(t: Tree[Int]): Int = t match {
		case Leaf(x) => x
		case Branch(l, r) => maximum(l) max maximum(r)
	}

	def depth[A](t: Tree[A]): Int = t match {
		case Leaf(x) => 1
		case Branch(l, r) => (depth(l) max depth(r)) + 1
	}

	def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
		case Leaf(x) => Leaf(f(x)) 
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
	}

	// def fold[A, B](t: Tree[A], r: B)(f: (Tree[A], Tree[B], B) => B): B = t match {
	// 	case _ => 
	// }
}

