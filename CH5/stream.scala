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

	def forAll(p: A => Boolean) = {
		this.foldRight(true)((a, r) => p(a) && r)
	}

	def takeWhileByFoldRight(p: A => Boolean): Stream[A] = {
		this.foldRight(Stream[A]())((a, r) => if (p(a)) Cons(() => a, () => r)
			                                  else Empty)
	}

	def map[B](f: A => B): Stream[B] = {
		this.foldRight(Stream[B]())((a, r) => Cons(()=>f(a), ()=>r))
	}

	def filter(f: A => Boolean) : Stream[A] = {
		this.foldRight(Stream[A]())((a, r) => if(f(a)) Cons(() => a, ()=>r)
	                                          else r)
	}

	def append[B >: A](s: => Stream[B]): Stream[B] = {
		this.foldRight(s)((a, r) => Cons(() => a, () => r))
	}

	def flatMap[B](f: A => Stream[B]): Stream[B] = {
		this.foldRight(Stream[B]())((a, s) => f(a).append(s))
	}

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream{
	def empty[A]: Stream[A] = Empty

	val ones: Stream[Int] = cons(1, ones)

	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}

	def apply[A](as: A*): Stream[A] = {
		if (as.isEmpty) Empty
		else Cons(() => as.head, () => apply(as.tail: _*))
	}

	def constant[A](a: A): Stream[A] = cons(a, constant(a))

	def from(n: Int): Stream[Int] = cons(n, from(n + 1))

	def fibs: Stream[Int] = {
		def go(a: Int, b: Int): Stream[Int] = {
			cons(a, go(b, a+b))
		}

		go(0, 1)
	}

	def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] = f(s) match {
		case Some((aa, ss)) => cons(aa, unfold(ss)(f))
		case _ => Empty 
	}

	def fibsByUnfold: Stream[Int] = unfold((0, 1))((t) => Option(t._1, (t._2, t._1 + t._2)))

	def fromByUnfold(n: Int): Stream[Int] = unfold(n)(t => Option(t, t+1))

	def constantByUnfold[A](a: A): Stream[A] = unfold(a)(t => Option(a, a))

	def onesByUnfold: Stream[Int] = constantByUnfold(1)

	def mapByUnfold[A, B](s: Stream[A])(f: A=>B): Stream[B] = {
		ff(s) => f(a) nextS
		unfold()
	}
}