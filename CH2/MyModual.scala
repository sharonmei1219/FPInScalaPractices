object MyModule{
	def abs(n: Int): Int =
		if (n < 0) -n
		else n

	def factorial(n: Int): Int = {
		def go(n: Int, acc: Int): Int =
			if (n <= 0) acc
			else go(n-1, acc*n)

		go(n, 1)
	}

	// it's simple but stupid at the same time
	def fib_stupidVersion(n: Int): Int = {
		def go(n: Int): Int = {
			if (n <= 1) 0
			else if (n == 2) 1
			else go(n-1) + go(n-2)
		}
		go(n)
	}

	def fib_improvedVersion(n: Int): Int = {
		var go_n_2: Int = 0
		var go_n_1: Int = 0

		def go(n: Int) : Int = {
			if (n <= 1) 0
			else if (n == 2) {
				go_n_2 = 0
				1
			}else{
				go_n_1 = go(n-1)
				val result = go_n_1 + go_n_2
				go_n_2 = go_n_1
				result
			}
		}

		go(n)
	}

	def fib_TCOVersion(n: Int): Int = {
		def go(n: Int, a: Int, b: Int): Int ={
			if (n <= 1) 0
			else if (n == 2) a
			else go(n-1, a+b, a)
		}
		go(n, 1, 0)
	}

	def partial1[A, B, C](a : A, f: (A, B) => C) : B => C = {
		(b:B) => f(a,b)
	}

	def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
		(a:A) => (b:B) => f(a, b)
	}

	def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
		(a: A, b: B) => f(a)(b)
	}

	def compose[A, B, C](f: B => C, g: A => B): A => C = {
		(a: A) => f(g(a))
	}

	def sum(a:Int, b:Int):Int = a+b
	private def formatAbs(x: Int) = {
		val msg = "The absolute value of %d is %d"
		msg.format(x, abs(x))
	}

	def formatResult(name: String, n: Int, f: Int => Int) = {
		val msg = "The %s of %d is %d."
		msg.format(name, n, f(n))
	}

	def main(args: Array[String]): Unit ={
		println(formatResult("absolute value", -42, abs))
		println(formatResult("factorial", 7, fib_TCOVersion))
		// val inc = partial1(1, +)
	}
		
}
