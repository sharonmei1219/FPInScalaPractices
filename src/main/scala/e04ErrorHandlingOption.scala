sealed trait Option[+A]{
	def map[B](f: A => B): Option[B] = this match{
		case None => None
		case Some(a) => Some(f(a))
	}

	def getOrElse[B >: A](default: => B): B = this match{
		case None => default
		case Some(a) => a
	}

	def flatMap[B](f: A => Option[B]): Option[B] =
		map(f) getOrElse None

	def orElse[B >: A](ob: => Option[B]): Option[B] = sys.error("todo")
	def filter(f: A => Boolean): Option[A] = sys.error("todo")
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


def mean(xs: Seq[Double]): Option[Double] = 
if (xs.isEmpty) None
else Some(xs.sum / xs.length)
