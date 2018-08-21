package fpinscala.errorhandling

import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
	def map[B](f: A=>B): Option[B] = this match {
		case Some(a) => Some(f(a))
		case _ => None
	}

	def flatMap[B](f: A => Option[B]): Option[B] = this match {
		case Some(x) => f(x)
		case _ => None
	}

	def getOrElse[B >: A](default: => B): B = this match {
		case Some(x) => x
		case _ => default
	}

	def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
		case Some(x) => this
		case _ => None
	}

	def filter(f: A => Boolean): Option[A] = this match {
		case Some(x) => {if (f(x)) this
		                 else None}
		case _ => None 
	}

}
case class Some[+A](x: A) extends Option[A]
case object None extends Option[Nothing]


object Option{

	def mean(xs: Seq[Double]): Option[Double] = {
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)
	}

	def variance(xs: Seq[Double]): Option[Double] = {
		mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
	}

	// def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C] = (a, b) match {
	// 	case (Some(x), Some(y)) => Some(f(x, y))
	// 	case _ => None
	// }

	def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C] = {
		b flatMap (y => a map (x => f(x, y)))
	}

	def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
		case Nil => Some(Nil)
		case h :: t =>  h flatMap (hh => sequence(t) map (tt => hh :: tt))
	}

	def sequenceByFoldRight[A](a: List[Option[A]]): Option[List[A]] = {
	 	a.foldRight[Option[List[A]]](Some(Nil))((h: Option[A], r:Option[List[A]]) => h flatMap (hh => r map (rr => hh :: rr)))
	}

	def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
		case Nil => Some(Nil)
		// case h :: t => traverse(t)(f) flatMap (tt => f(h) map(hh => hh::tt))
		case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
	}
}