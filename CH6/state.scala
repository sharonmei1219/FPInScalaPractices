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
}