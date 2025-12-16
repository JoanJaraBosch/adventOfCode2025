package util
import scala.annotation.tailrec
import scala.math.Integral.Implicits.infixIntegralOps
import scala.math.Ordering.Implicits.infixOrderingOps

object Integers {

  def naturalNumbers[T](implicit n: Integral[T]): LazyList[T] = {
    def next(x: T): LazyList[T] = {
      x #:: next(x + n.one)
    }

    next(n.one)
  }

  def factors(x: Int): Set[Int] = {
    val smallFactors = (1 to Math.sqrt(x).toInt)
      .filter(d => x % d == 0)
      .toSet

    val largeFactors = smallFactors.map(d => x / d)

    smallFactors union largeFactors
  }

  def lcm[T](x: T, y: T)(implicit n: Integral[T]): T = {
    x * (y / gcd(x, y))
  }

  def gcd[T](x: T, y: T)(implicit n: Integral[T]): T = {
    @tailrec
    def helper(a: T, b: T): T = {
      if (b == n.zero) a
      else helper(b, a % b)
    }

    if (x > y) helper(x, y)
    else helper(y, x)
  }

  /**
   * Compute the extended Euclidean algorithm for two integers
   * @param a First input
   * @param b Second input
   * @return The gcd, and bézout coefficients of a and b, in order
   */
  def extendedEuclidean[T](a: T, b: T)(implicit n: Integral[T]): (T, T, T) = {
    @tailrec
    def helper(oldR: T, r: T, oldS: T, s: T, oldT: T, t: T): (T, T, T) = {
      if (r == n.zero) (oldR, oldS, oldT)
      else {
        val quotient = oldR / r
        helper(r, oldR - quotient * r, s, oldS - quotient * s, t, oldT - quotient * t)
      }
    }

    if (a < b) {
      val result = extendedEuclidean(b, a)
      result.copy(_2 = result._3, _3 = result._2)
    }
    else helper(a, b, n.one, n.zero, n.zero, n.one)
  }

  def chineseRemainderTheorem[T](pairs: Vector[(T, T)])(implicit n: Integral[T]): T = {
    val (offset, multiplier) = pairs.reduce { (acc, next) =>
      val (accOffset, accMod) = acc
      val (nextOffset, nextMod) = next
      val (_, accBezout, nextBezout) = Integers.extendedEuclidean(accMod, nextMod)
      val result = accOffset * nextMod * nextBezout + nextOffset * accMod * accBezout
      (result, acc._2 * next._2)
    }

    val possibleResult = offset % multiplier
    if (possibleResult < n.zero) possibleResult + multiplier else possibleResult
  }

  // Debugging aid, not performant for large numbers
  def chineseRemainderTheoremSieve[T](pairs: Vector[(T, T)])(implicit n: Integral[T]): T = {
    def sequence(head: T, increment: T): LazyList[T] = {
      head #:: sequence(head + increment, increment)
    }

    @tailrec
    def helper(current: T, increment: T, remainingPairs: Vector[(T, T)]): T = {
      if (remainingPairs.isEmpty) current
      else {
        val (remainder, mod) = remainingPairs.head
        val nextAmount = sequence(current, increment).find(x => x % mod == remainder).get
        helper(nextAmount, increment * mod, remainingPairs.tail)
      }
    }

    val sorted = pairs.sorted(Ordering.by[(T, T), T](-_._2))

    helper(n.zero, n.one, sorted)
  }

  def exponentiationBySquaring[T](x: T, y: T)(implicit n: Integral[T]): T = {
    val two = n.one + n.one

    @tailrec
    def helper(z: T, x: T, y: T): T = {
      if (y == n.zero) z
      else if (y % two == n.zero) helper(z, x * x, y / two)
      else helper(x * z, x * x, (y - n.one) / two)
    }

    helper(n.one, x, y)
  }

}