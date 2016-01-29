sealed trait Tree[+A]
case class Leaf[+A](value: A) extends Tree[A]
case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
	def fold[A, B](tr:Tree[A])(fL:(A)=>B, fB: (B, B) => B):B = tr match{
		case Leaf(value) => fL(value)
		case Branch(left, right) => fB(fold(left)(fL, fB), fold(right)(fL, fB))
	}

	def size[A](tr:Tree[A]):Int = tr match {
		case Leaf(value) => 1
		case Branch(left, right) => size(left) + size(right) + 1
	}

	def sizeFold[A](tr:Tree[A]):Int = 
		fold(tr)(x => 1, (x:Int, y:Int) => x + y + 1)

	def maximum(tr:Tree[Int]):Int = tr match{
		case Leaf(value) => value;
		case Branch(left, right) => maximum(left) max maximum(right)
	}

	def maximumFold(tr:Tree[Int]):Int = 
		fold(tr)(x => x, (x:Int, y:Int) => x max y)

	def depth[A](tr:Tree[A]):Int = tr match {
		case Leaf(value) => 1;
		case Branch(left, right) => (depth(left) max depth(right)) + 1
	}

	def depthFold[A](tr:Tree[A]):Int = 
		fold(tr)(x => 1, (x:Int, y:Int) => (x max y) + 1)

	def map[A, B](tr:Tree[A])(f:A=>B):Tree[B] = tr match{
		case Leaf(value) => Leaf(f(value))
		case Branch(left, right) => Branch(map(left)(f), map(right)(f))
	}

}

val tree = Branch(Branch(Leaf[Int](3), Branch(Leaf[Int](2),Leaf[Int](5))), Leaf(1))
println(Tree.sizeFold(tree))
println(Tree.maximum(tree))
println(Tree.maximumFold(tree))
println(Tree.depth(tree))
println(Tree.depthFold(tree))
println(Tree.map(tree)(_+1))