package util

import scala.collection.immutable.NumericRange
import scala.language.implicitConversions
import scala.math.Integral.Implicits.infixIntegralOps

case class Complex[T](re: T, im: T)(implicit n: Integral[T]) extends Ordered[Complex[T]] {

val isReal: Boolean = im == n.zero

val isImaginary: Boolean = re == n.zero

def conjugate: Complex[T] = Complex(re, -im)

def +(x: Complex[T]): Complex[T] = Complex(this.re + x.re, this.im + x.im)
def -(x: Complex[T]): Complex[T] = Complex(this.re - x.re, this.im - x.im)
def *(x: Complex[T]): Complex[T] = Complex(this.re * x.re - this.im * x.im, this.im * x.re + this.re * x.im)
def /(x: Complex[T]): Complex[T] = {
val conj = x.conjugate
val d = this * conj
val q = x * conj
Complex(d.re / q.re, d.im / q.re)
  }
def unary_-(): Complex[T] = Complex(-re, -im)

def +(x: T): Complex[T] = this + Complex(x, n.zero)
def :+(x: T): Complex[T] = this + Complex(x, n.zero)
def *(x: T): Complex[T] = this * Complex(x, n.zero)
def :*(x: T): Complex[T] = this * Complex(x, n.zero)

/**
 * @return The normalised direction of the real or imaginary number (will be incorrect for complex numbers)
 */
def normalised: Complex[T] = Complex(if (re == n.zero) re else re / re.abs, if (im == n.zero) im else im / im.abs)

/**
 * @param other The given Complex
 * @return Manhattan distance between this and the given Complex
 */
def mh(other: Complex[T]): T = {
val diff = this - other
    n.abs(diff.re.abs + diff.im.abs)
  }

def to(end: Complex[T]): IndexedSeq[Complex[T]] = {
        NumericRange.inclusive(this.re, end.re, n.one)(n).flatMap { re =>
    NumericRange.inclusive(this.im, end.im, n.one)(n).map(im => Complex(re, im))
}
  }

/**
 * @return A complex which is the "lowest terms" form (which doesn't make sense, but hey ho)
 */
def simplify: Complex[T] = {
val (factor, _, _) = Integers.extendedEuclidean(n.abs(re), n.abs(im))
        if (factor == n.one) this
        else Complex(n.quot(re, factor), n.quot(im, factor))(n).simplify
  }

override def compare(that: Complex[T]): Int = {
        if (n.lt(im, that.im)) -1
        else if (n.gt(im, that.im)) 1
        else {
        if (n.lt(re, that.re)) -1
        else if (n.gt(re, that.re)) 1
        else 0
        }
        }
        }

        case object Complex {

def ZERO[T](implicit n: Integral[T]): Complex[T] = Complex(n.zero, n.zero)
def ONE[T](implicit n: Integral[T]): Complex[T] = Complex(n.one, n.zero)
def I[T](implicit n: Integral[T]): Complex[T] = Complex(n.zero, n.one)

def linearRange[T](start: Complex[T], end: Complex[T], step: Complex[T], inclusive: Boolean = false)(implicit n: Integral[T]): IndexedSeq[Complex[T]] = {
        if (inclusive) {
        if (start.re == end.re) {
        new NumericRange.Inclusive[T](start.im, end.im, step.im).map(im => Complex(start.re, im))
        }
        else if (start.im == end.im) {
        new NumericRange.Inclusive[T](start.re, end.re, step.re).map(re => Complex(re, start.im))
        }
        else throw new IllegalArgumentException(s"Non linear range $start to $end by $step")
    }
            else {
            if (start.re == end.re) {
        new NumericRange.Exclusive[T](start.im, end.im, step.im).map(im => Complex(start.re, im))
        }
        else if (start.im == end.im) {
        new NumericRange.Exclusive[T](start.re, end.re, step.re).map(re => Complex(re, start.im))
        }
        else throw new IllegalArgumentException(s"Non linear range $start to $end by $step")
    }
            }

trait ComplexIntIsIntegral extends Integral[Complex[Int]] {
def plus(x: Complex[Int], y: Complex[Int]): Complex[Int] = x + y
def minus(x: Complex[Int], y: Complex[Int]): Complex[Int] = x - y
def times(x: Complex[Int], y: Complex[Int]): Complex[Int] = x * y
def quot(x: Complex[Int], y: Complex[Int]): Complex[Int] = x / y
def rem(x: Complex[Int], y: Complex[Int]): Complex[Int] = x % y
def negate(x: Complex[Int]): Complex[Int] = -x
def fromInt(x: Int): Complex[Int] = Complex[Int](x, 0)
def parseString(str: String): Option[Complex[Int]] = ???
def toInt(x: Complex[Int]): Int = x.re
def toLong(x: Complex[Int]): Long = x.re
def toFloat(x: Complex[Int]): Float = x.re
def toDouble(x: Complex[Int]): Double = x.re
def compare(x: Complex[Int], y: Complex[Int]): Int = x.compare(y)
  }
implicit object ComplexIntIsIntegral extends ComplexIntIsIntegral {}

trait ComplexLongIsIntegral extends Integral[Complex[Long]] {
def plus(x: Complex[Long], y: Complex[Long]): Complex[Long] = x + y
def minus(x: Complex[Long], y: Complex[Long]): Complex[Long] = x - y
def times(x: Complex[Long], y: Complex[Long]): Complex[Long] = x * y
def quot(x: Complex[Long], y: Complex[Long]): Complex[Long] = x / y
def rem(x: Complex[Long], y: Complex[Long]): Complex[Long] = x % y
def negate(x: Complex[Long]): Complex[Long] = -x
def fromInt(x: Int): Complex[Long] = Complex[Long](x, 0)
def parseString(str: String): Option[Complex[Long]] = ???
def toInt(x: Complex[Long]): Int = x.re.toInt
def toLong(x: Complex[Long]): Long = x.re
def toFloat(x: Complex[Long]): Float = x.re
def toDouble(x: Complex[Long]): Double = x.re
def compare(x: Complex[Long], y: Complex[Long]): Int = x.compare(y)
  }
implicit object ComplexLongIsIntegral extends ComplexLongIsIntegral {}

}