package fpinscala.laziness

sealed trait Stream[+A]{

	def toList: List[A] = {
		def go(r: List[A] => List[A], s: Stream[A]): List[A] = s match {
			case Cons(h, t) => go(l => r(h()::l), t())
			case _ => r(Nil)
		}
		go(l => l, this)
	}

	def take(n: Int) : Stream[A] = {
		def go(r: Stream[A] => Stream[A], c: Int, t: Stream[A]): Stream[A] = t match {
			case Cons(h, t) => if (c == 0) r(Empty)
							   else go(ss => r(Cons(h, () => ss)), c - 1, t())
			case Empty => Empty
		}

		go(s => s, n, this)
	}

	def takeWhile(p: A => Boolean): Stream[A] = {
		def go(r: Stream[A] => Stream[A], t: Stream[A]): Stream[A] = t match {
			case Cons(h, t) => if(p(h())) go(ss => r(Cons(h, () => ss)), t())
			                   else r(Empty)
			case Empty => Empty
		}

		go(s => s, this)
	}

	def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
		case Cons(h, t) => f(h(), t().foldRight(z)(f))
		case Empty => z
	}

	

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream{
	def apply[A](as: A*): Stream[A] = {
		if (as.isEmpty) Empty
		else Cons(() => as.head, () => apply(as.tail: _*))
	}
}