// package fpinscala.laziness

sealed trait Stream[+A]{
	
	def toListRecursive: List[A] = this match {
		case Cons(h, t) => h() :: t().toListRecursive
		case _ => Nil
	}

	def toList: List[A] = {
		def go(r: List[A] => List[A], s: Stream[A]): List[A] = s match {
			case Cons(h, t) => go((l: List[A]) => r(h()::l), t())
			case _ => r(Nil)
		}
		go(l => l, this)
	}

	def take(n: Int) : Stream[A] = {
		def go(r: Stream[A] => Stream[A], c: Int, t: Stream[A]): Stream[A] = t match {
			case Cons(h, t) => if (c == 0) r(Empty)
							   else	go(ss => r(Cons(h, () => ss)), c - 1, t())
			case Empty => r(Empty)
		}
		go(s => s, n, this)
	}

	def takeWhile(p: A => Boolean): Stream[A] = {
		def go(r: Stream[A] => Stream[A], t: Stream[A]): Stream[A] = t match {
			case Cons(h, t) => if (p(h())) go(ss => r(Cons(h, ()=>ss)), t())
							   else r(Empty)
			case Empty => r(Empty) 
		}
		go(s => s, this)
	}

	def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
		case Cons(h, t) => f(h(), t().foldRight(z)(f))
		case Empty => z 
	}

	def forAll(p: A => Boolean): Boolean = {
		this.foldRight(true)((a, r) => p(a) && r)
	}

	def takeWhileByfoldRight(p: A => Boolean): Stream[A] = {
		this.foldRight(Stream[A]())((a, r) => if (p(a)) Cons(() => a, () => r)
										      else Cons(() => a, () => Empty))
	}

	def map[B](f: A => B): Stream[B] = {
		this.foldRight(Stream[B]())((a, r) => Cons(() => f(a), () => r))
	}

	def filter(f: A => Boolean) : Stream[A] = {
		this.foldRight(Stream[A]())((a, r) => if (f(a)) Cons(() => a, () => r)
			                                  else r)
	}

// 	// def append(s: Stream[A]): Stream[A] = {
// 	// 	// this.foldRight(s)((a, r）=> Cons(() => a, () => r))
// 	// 	this.foldRight(s)((a,r)=>Cons(()=>a,()=>r))
// 	// }
	
// 	// def flatMap[B](f: A => Stream[B]): Stream[B] = {
// 	// 	this.foldRight[Stream[B]](Stream[B]())((a, r) => f(a).append(r))
// 	// }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

// object Stream{
// 	def apply[A](as: A*): Stream[A] ={
// 		if (as.isEmpty) Empty
// 		else Cons(() => as.head, () => apply(as.tail: _*))
// 	}
// }
