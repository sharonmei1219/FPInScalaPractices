// --- 2.5 ---
def compose[A, B, C](f:B=>C, g:A=>B):A=>C = {
	a:A => f(g(a))
}

// --- 2.4 ---
def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
	(a:A, b:B) => f(a)(b)
}

// --- 2.3 ---
def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
	a:A => b:B => f(a, b)
}

// --- 2.2 ---
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
	@annotation.tailrec
	def loop(n:Int):Boolean =
		if (!ordered(as(n), as(n-1)))	false
		else if (n == as.length) true
		else loop(n+1)

	loop(1)
}