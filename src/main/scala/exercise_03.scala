package DataStructure

sealed trait List[+A]
case object Nil extends List[Nothing]
//case class: A case class is an instantiable class that includes 
//several automatically generated methods
//Case classes work great for data transfer objects, the kind of classes 
//that are mainly used for storing data, given the data-based methods that are generated.
//Ref: "Learning Scala" (safari books online)

case class Cons[+A](head: A, tail: List[A]) extends List[A]
//object singleton class
//object List companion object: A companion object is an object with the same
//as a class or trait and is defined in the same source file as the associated
//file or trait. A companion object differs from other
//objects as it has access rights to the class/trait that other objects do not
object List{
	def sum(ints: List[Int]): Int = ints match{
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match{
		case Nil => 1.0
		case Cons(0.0, _) => 0.0 //"_" matches any expression
		case Cons(x, xs) => x * product(xs)
	}

	//exercise 3.2
	def tail[A](ds: List[A]): List[A] = ds match {
		case Nil => Nil
		case Cons(_, xs) => xs 
	}

	//exercise 3.3
	def setHead[A](head: A, ds: List[A]): List[A] = ds match {
		case Nil => Nil
		case Cons(_, xs) => Cons(head, xs)
	}

	//exercise 3.4
	def drop[A](n: Int, ds: List[A]): List[A] = 
	    if (n <= 0) ds
	    else ds match {
	    	case Nil => Nil
	    	case Cons(_, xs) => drop(n-1, xs) 
	    }

	//exercise 3.5
    def dropWhile[A](l: List[A], f: A => Boolean) : List[A] = l match{
    	case Cons(h, t) if f(h) => dropWhile(t, f)
    	case _ => l
    }

    //exercise 3.6
    def init[A](l: List[A]): List[A] = l match {
    	case Nil => Nil
    	case Cons(_, Nil) => Nil
    	case Cons(h, t) => Cons(h, init(t))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    	case Nil => z
    	case Cons(x, xs) => f(x, foldRight(xs, z)(f)) 
    }

    //exercise 3.10
    def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    	case Nil => z
    	case Cons(x, xs) => foldLeft(xs, f(x, z))(f) 
    }

    //exercise 3.9
    def length[A](as: List[A]): Int =
        foldRight(as, 0)((_, c) => c+1)

    //exercise 3.12
    def revert[A](as: List[A]): List[A] = 
    	foldLeft(as, Nil:List[A])(Cons(_,_))

    //exercise 3.13
    def foldRightTailRecursive[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    	foldLeft(revert(as), z)(f)
    }
    //f(a0, f(a1, f(a2, z)))
    def foldRightTailRecursive_1[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    	foldLeft(as, (b:B) => b)((a, g) => (b:B) => g(f(a,b)))(z)
    }

    //exercise 3.14
    def appendViaFoldRight[A](z: List[A], as: List[A]): List[A] = {
    	foldRight(as, z)(Cons(_,_))
    }

    //exercise 3.15
    def flat[A](ll: List[List[A]]): List[A] = {
    	foldLeft(ll, List[A]())(appendViaFoldRight(_, _))
    }

    //exercise 3.16
    def addOne(as: List[Int]): List[Int] = as match {
    	case Nil => Nil
    	case Cons(h, t) => Cons(h + 1, addOne(t)) 
    }

    def addOne_1(as: List[Int]): List[Int] =
    	foldRightTailRecursive(as, Nil:List[Int])((x, l) => Cons(x + 1, l))

    //exercise 3.17
    def toString(as: List[Double]): List[String] =
        foldRightTailRecursive(as, Nil:List[String])((x, l) => Cons(x.toString(), l))

    //exercise 3.18
    def map[A, B](as: List[A], f: (A) => B): List[B] = {
    	foldRight(as, List[B]())((x, y) => Cons(f(x), y))
    }

    //exercise 3.19
    def filter[A](as: List[A], f: (A) => Boolean) = {
    	foldRight(as, List[A]())((x, y) => if (f(x)) Cons(x, y) else y)
    }

    //exercise 3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    	flat(map(as, f))
    }

    //exercise 3.21
    def filterViaFlatMap[A](as: List[A])(f: (A) => Boolean) = {
    	flatMap(as)((x:A) => if (f(x)) List[A](x) else Nil:List[A])
    }

    //exercise 3.22
    def addList(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    	case (_, Nil) => Nil
    	case (Nil, _) => Nil
    	case (Cons(ah, at), Cons(bh, bt)) => Cons(ah + bh, addList(at, bt)) 
    }

    //exercise 3.23
    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C):List[C] = (as, bs) match {
    	case (_, Nil) => Nil
    	case (Nil, _) => Nil
    	case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f)) 
    }

	//so List can also initialized by List(1, 2, 3) 
	//besides Cons(1, Cons(2, Nil))
	//It is a variadic function, meaning it accepts zero or more arguments of type A
	//Variadic functions are just providing a little syntactic sugar 
	//for creating and passing a Seq of elements explicitly.
	//Seq[A] base of list, queue, etc. which has head and tail functions
	//The special _* type annotation allows us to pass a Seq to a variadic method
	def apply[A](as: A*): List[A] =
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
}

sealed trait Tree[+A]
case class Leaf[+A](value: A) extends Tree[A]
case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
	//exercise 3.25
	def size[A](tree:Tree[A]): Int = tree match {
		case Leaf(_) => 1
		case Branch(l, r) => 1 + size(l) + size(r) 
	}
	
	//exercise 3.26
	def maximum(tree:Tree[Int]): Int = tree match {
		case Leaf(x) => x
		case Branch(l, r) => maximum(l) max maximum(r) 
	}

	//exercise 3.27
	def depth[A](tree:Tree[A]): Int = tree match {
		case Leaf(_) => 1
		case Branch(l, r) => depth(l) max depth(r) + 1
	}

	//exercise 3.28
	def map[A, B](tree:Tree[A])(f:(A) => B):Tree[B] = tree match {
		case Leaf(x) => Leaf(f(x))
		case Branch(l, r) => Branch(map(l)(f), map(r)(f)) 
	}
}

// val ex1: List[Int] = Nil
// val ex2: List[Int] = Cons(1, Cons(2, Nil))
// val ex3 = List(1, 2, 3)
// //exercise 3.3
// // println(List.setHead(5, ex3))

// //exercise 3.12
// println(List.revert(ex3))
