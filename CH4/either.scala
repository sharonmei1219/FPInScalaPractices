package fpinscala.errorhandling

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}
sealed trait Either[+E, +A]{
	def map[B](f: A=>B) : Either[E, B] = this match {
		case Right(a) => Right(f(a))
		case Left(e) => Left(e)
	}

	def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
		case Right(a) => f(a)
		case Left(e) => Left(e) 
	}

	def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
		case Right(a) => Right(a) 
		case _ => b
	}

	def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
		// this flatMap (aa => b map (bb => f(aa, bb)))
		for { a <- this; b1 <- b } yield f(a,b1)

	}
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either{
	def traverse[E, A, B](a: List[A])(f: A=>Either[E, B]): Either[E, List[B]] = a match {
		case Nil => Right(Nil)
		// case h :: t => f(h) flatMap (hh => traverse(t)(f) map (tt => hh :: tt)) 
		case h :: t => (f(h) map2 traverse(t)(f))( _:: _) 
	}
}