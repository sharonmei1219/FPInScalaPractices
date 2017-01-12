sealed trait Option[+A]{
	def map[B](f: A => B): Option[B] = this match {
		case None => None
		case Some(x) => Some(f(x)) 
	}

	def getOrElse[B>:A](default: => B) : B = this match {
		case None => default
		case Some(x) => x
	}

	def flatMap[B](f: A => Option[B]): Option[B] = 
		map(f) getOrElse None

	def orElse[B>:A](ob: => Option[B]): Option[B] = 
		map(Some(_)) getOrElse ob

	def filter(f: A => Boolean): Option[A] =
		flatMap(x => if(f(x)) Some(x) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


// def mean(xs: Seq[Double]): Option[Double] =
//     if (xs.isEmpty) None
//     else Some(xs.sum / xs.length)

// //exercise 4.2
// def variance(xs: Seq[Double]): Option[Double] = 
// 	mean(xs) flatMap(m => mean(xs map(x => math.pow(x - m, 2))))

// //exercise 4.3
// def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C] =
// 	b flatMap(x => a map (f(_, x)))

// //exercise 4.4
// def sequence[A](a: List[Option[A]]): Option[List[A]] =
// 	a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_::_))

// //exercise 4.5
// def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
// 	a.foldRight[Option[List[B]]](Some(Nil))((x, r) => map2(f(x), r)(_::_))