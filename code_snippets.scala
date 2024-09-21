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
