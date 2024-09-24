// Square-Root function from 1.3
def sqrt(x: Double) = {
	def sqrtIter(guess: Double): Double = 
		if isGoodEnough(guess) then guess
		else sqrtIter(improve(guess))
	
	def improve(guess: Double) =
		(guess + (x / guess)) / 2
	
	def isGoodEnough(guess: Double) =
		// delta because of floating point
		abs((guess * guess) - x) < 0.001

	sqrtIter(1.0)
}

// ==========================================================

// Higher-order Sum function from 2.1
def sum(f: Int => Int, a: Int, b: Int): Int =
	if a > b then 0
	else f(a) + sum(f, a + 1, b)

// Curried Sum function from 2.2
def sum(f: Int => Int) (a: Int, b: Int): Int = 
	if a > b then 0 else f(a) + sum(f)(a + 1, b)

def sumInts  = sum(x => x)
def sumCubes = sum(x => x * x * x)

// ==========================================================

// Rational class from 2.3 and 2.4
class Rational(x: Int, y: Int):
	require(y > 0, "denominator must be positive")
	def this(x: Int) = this(x, 1)

	private def gcd(a: Int, b: Int): Int =
		if b == 0 then a else gcd(b, a % b)
	private val g = gcd(x, y)

	val numer = x / g
	val denom = y / g

	def add(that: Rational) =
		Rational(
			this.numer * that.denom + that.numer * this.denom,
			this.denom * that.denom
		)

	def mul(that: Rational) = ...
	...
	override def toString = s"$numer/$denom"
end Rational

extension (r: Rational)
	def min(s: Rational): Rational = if s.less(r) then s else r
	def abs: Rational = Rational(r.numer.abs, r.denom)

extension (x: Rational)
	def + (y: Rational): Rational = x.add(y)
	def * (y: Rational): Rational = x.mul(y)

// ==========================================================

// IntSet class hierarchy from 3.1
abstract class IntSet:
	def contains(x: Int): Boolean
	def including(x: Int): IntSet

object Empty extends IntSet:
	def contains(x: Int): Boolean = false
	def including(x: Int): IntSet = NonEmpty(s, Empty(), Emtpy())
end Empty

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:
	def contains(x: Int): Boolean =
		if x < elem then left.contains(x)
		else if x > elem then right.contains(x)
		else true

	def incl(x: Int): IntSet =
		if x < elem then NonEmpty(elem, left.incl(x), right)
		else if x > elem then NonEmpty(elem, left, right.incl(x))
		else this
end NonEmpty

// ==========================================================

// Expr class hierarchy from 3.2 and 3.3 - Solution 1
trait Expr:
	def eval: Int

class Number(n: Int) extends Expr:
	def eval: Int = n

class Sum(e1: Expr, e2: Expr) extends Expr:
	def eval: Int = e1.eval + e2.eval

// Solution 2
enum Expr:
	case Var(s: String)
	case Number(n: Int)
	case Sum(e1: Expr, e2: Expr)
	case Prod(e1: Expr, e2: Expr)

def eval(e: Expr): Int = e match
	case Expr.Number(n)    => n
	case Expr.Sum(e1, e2)  => eval(e1) + eval(e2)
  case Expr.Var(x)       => ...
  case Expr.Prod(e1, e2) => ...

def show(e: Expr): String = e match
	case Expr.Var(x) => x
	case Expr.Number(n) => n.toString
	case Expr.Sum(a, b) => s”${show(a)} + ${show(a)}}”
	case Expr.Prod(a, b) => s”${showP(a)} * ${showP(a)}”

def showP(e: Expr): String = e match
	case sum: Expr.Sum => s”(${show(sum)})”
	case _ => show(e)

// ==========================================================

// direction enum from 3.3
enum Direction(val dx: Int, val dy: Int):
	case Up extends Direction( 0, 1)
	case Right extends Direction( 1, 0)
	case Down extends Direction( 0, -1)
	case Left extends Direction(-1, 0)

	def rightTurn = Direction.values((ordinal + 1) % 4)
end Direction
