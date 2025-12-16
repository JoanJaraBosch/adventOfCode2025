package util

import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Numeric.IntIsIntegral

/**
 * Grid utility class
 *
 * The address (0, 0) is the top left hand corner of the grid
 *
 * @param values The grid vectors
 * @param wrapping True if the grid wraps around; false otherwise
 * @tparam T The type of value in the grid
 */
case class Grid[T](values: Vector[Vector[T]], wrapping: Boolean = false) {

  val rowLength: Int = values.head.size
  val columnLength: Int = values.size
  val centre: Complex[Int] = Complex(values.head.size / 2, -(values.size / 2))

  def apply(address: Complex[Int]): T = {
    values(-address.im)(address.re)
  }

  def getOrElse(address: Complex[Int], defaultValue: T): T = {
    if (address.re >= 0 && address.im <= 0) this(address) else defaultValue
  }

  def updated(address: Complex[Int], value: T): Grid[T] = {
    Grid(values.updated(-address.im, values(-address.im).updated(address.re, value)), wrapping)
  }

  def updated(address: Complex[Int], grid: Grid[T]): Grid[T] = {
    grid.zipWithIndex.map(p => (p._1, p._2 + address)).foldLeft(this) {(self, other) =>
      self.updated(other._2, other._1)
    }
  }

  def slice(address: Complex[Int], width: Int, height: Int): Grid[T] = {
    Grid(values.slice(-address.im, -address.im + height).map { row =>
      row.slice(address.re, address.re + width)
    }, wrapping)
  }

  def summedArea[B >: T](implicit num: Numeric[B]): Grid[B] = {
    indices.foldLeft(Grid.of(columnLength, rowLength, num.zero, wrapping)) { (acc, c) =>
      acc.updated(c, acc.getOrElse(c - Complex.ONE(IntIsIntegral), num.zero) + acc.getOrElse(c + Complex.I(IntIsIntegral), num.zero) - acc.getOrElse(c - Complex.ONE(IntIsIntegral) + Complex.I(IntIsIntegral), num.zero) + this(c))
    }
  }

  def neighboursWithDiagonals(address: Complex[Int]): IndexedSeq[Complex[Int]] = {
    if (wrapping) {
      throw new UnsupportedOperationException("Wrapping grid not yet supported")
    }
    else for {
      i <- -1 to 1
      j <- -1 to 1
      if !(i == 0 && j == 0)
      if (address.re + i) >= 0 && (address.re + i) < rowLength
      if (address.im + j) <= 0 && (address.im + j) > -columnLength
    } yield Complex(address.re + i, address.im + j)
  }

  def neighbours(address: Complex[Int]): IndexedSeq[Complex[Int]] = {
    if (wrapping) {
      throw new UnsupportedOperationException("Wrapping grid not yet supported")
    }
    else for {
      i <- -1 to 1
      j <- -1 to 1
      if !(i == 0 && j == 0)
      if i == 0 || j == 0
      if (address.re + i) >= 0 && (address.re + i) < rowLength
      if (address.im + j) <= 0 && (address.im + j) > -columnLength
    } yield Complex(address.re + i, address.im + j)
  }

  def diagonals(address: Complex[Int]): IndexedSeq[Complex[Int]] = {
    if (wrapping) {
      throw new UnsupportedOperationException("Wrapping grid not yet supported")
    }
    else for {
      i <- -1 to 1
      j <- -1 to 1
      if i != 0 && j != 0
      if (address.re + i) >= 0 && (address.re + i) < rowLength
      if (address.im + j) <= 0 && (address.im + j) > -columnLength
    } yield Complex(address.re + i, address.im + j)
  }

  def next(address: Complex[Int], delta: Complex[Int]): Complex[Int] = {
    val possibleNext = address + delta

    if (wrapping) {
      val result = Complex(
        (possibleNext.re % rowLength + rowLength) % rowLength,
        -((-possibleNext.im % columnLength + columnLength) % columnLength)
      )
      result
    }
    else {
      if (possibleNext.re < 0 || possibleNext.re >= rowLength
        || possibleNext.im > 0 || possibleNext.im <= -columnLength) address
      else possibleNext
    }
  }

  def grouped(rows: Int, columns: Int): Grid[Grid[T]] = {
    val groupedRows = values.map(_.grouped(columns).toVector)
    val rowGroups = groupedRows.grouped(rows).toVector
    val gridRows = rowGroups.map { rowGroup =>
      rowGroup.transpose.map(Grid(_, wrapping))
    }
    Grid(gridRows)
  }

  def rotateRight(): Grid[T] = Grid(values.transpose.map(_.reverse), wrapping)
  def rotateLeft(): Grid[T] = Grid(values.map(_.reverse).transpose, wrapping)

  def flipHorizontally(): Grid[T] = Grid(values.map(_.reverse), wrapping)
  def flipVertically(): Grid[T] = Grid(values.reverse, wrapping)

  def firstRow: Vector[T] = values.head
  def lastRow: Vector[T] = values.last
  def firstColumn: Vector[T] = values.map(_.head)
  def lastColumn: Vector[T] = values.map(_.last)

  def rotateRow(row: Int, by: Int): Grid[T] = {
    if (by == 0) return this

    val affectedRow = values(row)

    if (by < 0) {
      throw new UnsupportedOperationException("Rotate left not supported")
    }
    else {
      val toMove = affectedRow.slice(affectedRow.size - by, affectedRow.size)
      val newRow = toMove ++ affectedRow.slice(0, affectedRow.size - by)
      Grid(values.updated(row, newRow), wrapping)
    }
  }

  def rotateColumn(column: Int, by: Int): Grid[T] = {
    if (by == 0) return this

    val affectedColumn = values.map(r => r(column))

    if (by < 0) {
      // Remember by is -ve
      val toMove = affectedColumn.slice(affectedColumn.size + by, affectedColumn.size)
      val newColumn = toMove ++ affectedColumn.slice(0, affectedColumn.size + by)
      Grid(values.zip(newColumn).map(p => p._1.updated(column, p._2)), wrapping)
    } else {
      throw new UnsupportedOperationException("Rotate up not supported")
    }
  }

  def map[S](f: T => S): Grid[S] = {
    Grid(values.map(row => row.map(f)), wrapping)
  }

  // The standard zipWithIndex assumes integer indices
  def zipWithIndex: Grid[(T, Complex[Int])] = {
    Grid(values.zipWithIndex.map { case (row, im) =>
      row.zipWithIndex.map {
        case (v, re) => (v, Complex(re, -im))
      }
    }, wrapping)
  }

  def indices: Vector[Complex[Int]] = {
    values.zipWithIndex.flatMap { case (row, im) =>
      row.zipWithIndex.map {
        case (_, re) => Complex(re, -im)
      }
    }
  }

  def count(p: T => Boolean): Int = {
    values.map(row => row.count(p)).sum
  }

  def foldLeft[S](value: S)(f: (S, T) => S): S = {
    values.foldLeft(value) { (current, row) =>
      row.foldLeft(current)((current, t) => f(current, t))
    }
  }

  def indexOf(x: T): Option[Complex[Int]] = {
    zipWithIndex.values.flatten.find {
      case (v, _) => x == v
    }.map(_._2)
  }

  def indexWhere(p: T => Boolean): Option[Complex[Int]] = {
    zipWithIndex.values.flatten.find {
      case (v, _) => p(v)
    }.map(_._2)
  }

  def filter(p: T => Boolean): Seq[T] = {
    values.flatten.filter(p)
  }

  def find(p: T => Boolean): Option[T] = {
    values.flatten.find(p)
  }

  def exists(p: T => Boolean): Boolean = {
    values.flatten.exists(p)
  }

  def forall(p: T => Boolean): Boolean = {
    values.flatten.forall(p)
  }

  def size: Int = {
    rowLength * columnLength
  }

  def toMap: Map[Complex[Int], T] = {
    indices.map(i => i -> apply(i)).toMap
  }

  override def toString: String = {
    val gridString = values.map(row => row.mkString("")).mkString("\n")

    s"Grid(\n$gridString\n)"
  }
}

case object Grid {
  def of[T](rows: Int, columns: Int, value: T, wrapping: Boolean = false): Grid[T] = {
    Grid(
      Vector.fill(rows)(Vector.fill(columns)(value)),
      wrapping
    )
  }

  def flatten[T](input: Grid[Grid[T]], wrapping: Boolean = false): Grid[T] = {
    Grid(input.values.flatMap { gridRow =>
      gridRow.map(_.values).reduce { (a, b) =>
        a.zip(b).map(p => p._1 ++ p._2)
      }
    }, wrapping)
  }

  /**
   * Pad lines to equal length to better facilitate construction of a Grid
   * @param input The input lines to pad
   * @return Lines padded to equal length
   */
  def padLines(input: Vector[String]): Vector[String] = {
    val maxLineLength = input.maxBy(_.length).length

    input.map { line =>
      if (line.length == maxLineLength) line
      else line + (" " * (maxLineLength - line.length))
    }
  }

  /**
   *
   * @param summedArea A grid produced by calling areaSum on another grid
   * @param address The top left corner of the region
   * @param width The width of the region
   * @param height The height of the region
   * @return
   */
  def areaSum[T](summedArea: Grid[T], address: Complex[Int], width: Int, height: Int)(implicit num: Numeric[T]): T = {
    val bottomRightAddress = address + Complex(width - 1, -(height - 1))
    val topRightAddress = address + Complex(width - 1, 1)
    val bottomLeftAddress = address + Complex(-1, -(height - 1))
    val topLeftAddress = address + Complex(-1, 1)

    val bottomRight = summedArea(bottomRightAddress)
    val topRight = summedArea.getOrElse(topRightAddress, num.zero)
    val bottomLeft = summedArea.getOrElse(bottomLeftAddress, num.zero)
    val topLeft = summedArea.getOrElse(topLeftAddress, num.zero)

    bottomRight - topRight - bottomLeft + topLeft
  }

  /**
   * @return An Ordering of grid indices in reading order (left to right, top to bottom)
   */
  def ordering: Ordering[Complex[Int]] = { (x: Complex[Int], y: Complex[Int]) =>
    if (x.im < y.im) 1
    else if (x.im > y.im) -1
    else {
      if (x.re < y.re) -1
      else if (x.re > y.re) 1
      else 0
    }
  }
}