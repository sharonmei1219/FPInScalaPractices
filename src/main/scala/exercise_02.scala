object MyModule{
	def abs(n: Int): Int =
		if (n < 0) -n
		else n

	private def formatAbs(x: Int) = {
		val msg = "The absolute value of %d is %d"
		msg.format(x, abs(x))
	}

	def factorial(n: Int): Int = {
		def go(n: Int, acc: Int): Int =
		    if (n <= 0) acc
		    else go(n-1, n*acc)

		go(n, 1)
	}

	//exercise 2.1 O(n)
	def fib(n: Int): Int = {
		def go(n: Int, acc0: Int, acc1:Int): Int =
		    if (n == 1) acc0
		    else if (n == 2) acc1
		    else go(n-1, acc1, acc1 + acc0)
		go(n, 0, 1)
	}

	//O(2^n)
	def fibAwful(n: Int): Int = {
		def go(n: Int): Int =
		    if (n == 1) 0
		    else if (n == 2) 1
		    else go(n-1) + go(n-2)
		go(n)
	}

	//exercise 2.2
	def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
	    def go(n: Int): Boolean =
	        if (n >= as.length) true
	        else if (!ordered(as(n), as(n-1))) false
	        else go(n+1)

	    go(1)
	}

	//exercise 2.3
	def curry[A, B, C](f: (A, B) => C): A => (B=>C) =
	    (a: A) => ((b: B) => f(a, b))

	//exercise 2.4
	def uncurry[A, B, C](f: A => B => C): (A, B) => C =
	    (a:A, b:B) => f(a)(b)

	//exercise 2.5
	def compose[A, B, C](f: B => C, g: A => B): A => C =
	    (a: A) => f(g(a))

	def main(args: Array[String]): Unit = {
		// println(formatAbs(-42))

		//exercise 2.1
		// println(fib(20))

		//exercise 2.2
		// println(isSorted(Array(1, 2, 3), (x: Int, y: Int) => x >= y))

		//exercise 2.3
		// val inc = curry((a: Int, b: Int) => a+b)(1)
		// println(inc(6))

		//exercise 2.4
		// val add = uncurry(curry((a:Int, b:Int) => a+b))
		// println(add(2, 3))

		//exercise 2.5
		var inc = compose((a:Int) => a - 2, (a:Int) => a + 3)
		println(inc(5))
	}
}
