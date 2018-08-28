package fpinscala.state
import Stream._

trait RNG {
	def nextInt: (Int, RNG)
}

object RNG{
	def simple(seed: Long): RNG = new RNG {
		def nextInt = {
			val seed2 = (seed * 0x5DEECE66DL + 0xBL) &((1L << 48) - 1)
			((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
		}
	}

	def positiveInt(rng: RNG): (Int, RNG) = {
		val (i, r) = rng.nextInt
		(i.abs, r)
	}

	def double(rng: RNG): (Double, RNG) = {
		val (i, r) = positiveInt(rng)
		(i/(Int.MaxValue.toDouble + 1), r)
	}

	def intDouble(rng: RNG): ((Int, Double), RNG) = {
		val (i, r1) = rng.nextInt
		val (d, r2:RNG) = double(r1)
		((i, d), r2)
	}

	def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

		if(count == 0){
			(List(), rng)
		}else{
			val (i, nr) = rng.nextInt
			val (l, nnr) = ints(count - 1)(nr)
			(i::l, nnr)
		}
	}

	// TOC Version
	def intsTOCVersion(count: Int)(rng: RNG): (List[Int], RNG) = {
		def go(c: Int, rng: RNG, l: List[Int] => List[Int]): (List[Int], RNG) = {
			if(c == 0){
				(l(List()), rng)
			}else{
				val (i, nr) = rng.nextInt
				go(c - 1, nr, ll => l(i::ll))
			}
		}

		go(count, rng, l => l)
	}

	type Rand[+A] = RNG => (A, RNG)

	val int: Rand[Int] = _.nextInt

	def unit[A](a: A): Rand[A] = rng => (a, rng)

	def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => { 
		val (a, rng2) = s(rng)
		(f(a), rng2)
	}

	def positiveMax(n: Int): Rand[Int] = {
		map(double)(a => (a*n).toInt)
	}

	def double2 : Rand[Double] = map(positiveInt)(_/(Int.MaxValue.toDouble+1))

	def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
		rng => {
			val (a, r1) = ra(rng)
			val (b, r2) = rb(r1)
			(f(a, b), r2)
		}
	}

	def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
		fs.foldRight(unit(List[A]()))((f, r) => map2(f, r)(_::_))
		// fs.foldRight(rng => (unit(List[A]()))((a: Rand[A], r) => {
		// 	nr => {
		// 		val (aa, rr) = a(nr)
		// 		(aa::r._1, rr)
		// 	}
		// })



		// rng => {
		// 	def go(fs: List[Rand[A]], fl: List[A] => List[A], nr: RNG) => fs match {
		// 		case h::t => {	
		// 				(aa, rr) = h(nr)
		// 				go(t, ll => fl(aa::ll), rr)
		// 			}
		// 		case _ => (fl(List[A]()), nr)
		// 	}

		// 	go(fs, l => l, rng)
		// }
	}
}