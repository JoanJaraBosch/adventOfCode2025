package aoc

import util.Input

object Day04 {

  private def inBounds(r: Int, c: Int, rows: Int, cols: Int): Boolean =
    r >= 0 && r < rows && c >= 0 && c < cols

  private val deltas = List((-1, -1), (-1, 0), (-1, 1), (0, -1),(0, 1), (1, -1),  (1, 0), (1, 1))

  private def countAccessibleAndRender(lines: List[String]): (Int, List[String]) = {
    if (lines.isEmpty) return (0, Nil)

    val rows = lines.length
    val cols = lines.map(_.length).max

    // Convertir a grid rectangular de Char, rellenando con '.' si faltase
    val grid: Array[Array[Char]] = Array.ofDim[Char](rows, cols)
    for (r <- 0 until rows) {
      val line = lines(r)
      for (c <- 0 until cols) {
        grid(r)(c) = if (c < line.length) line.charAt(c) else '.'
      }
    }

    // Resultado marcado (copiar grid y sustituir accesibles por 'x')
    val rendered: Array[Array[Char]] = Array.tabulate(rows, cols){ (r, c) => grid(r)(c) }

    var accessibleCount = 0

    for {
      r <- 0 until rows
      c <- 0 until cols
      if grid(r)(c) == '@'
    } {
      // contar vecinos '@'
      val neighbors = deltas.count { case (dr, dc) =>
        val rr = r + dr
        val cc = c + dc
        inBounds(rr, cc, rows, cols) && grid(rr)(cc) == '@'
      }

      if (neighbors < 4) {
        accessibleCount += 1
        rendered(r)(c) = 'x'
      } else {
        // dejamos '@' tal cual
      }
    }

    val renderedLines = rendered.map(_.mkString).toList
    (accessibleCount, renderedLines)
  }

  private def accessiblePositions(grid: Array[Array[Char]]): List[(Int, Int)] = {
    val rows = grid.length
    val cols = grid(0).length

    (for {
      r <- 0 until rows
      c <- 0 until cols
      if grid(r)(c) == '@'
    } yield {
      val neighbors = deltas.count { case (dr, dc) =>
        val rr = r + dr
        val cc = c + dc
        inBounds(rr, cc, rows, cols) && grid(rr)(cc) == '@'
      }
      if (neighbors < 4) Some((r, c)) else None
    }).flatten.toList
  }

  def part1(input: List[String]): Int = {
    val (count, _) = countAccessibleAndRender(input)
    count
  }

  def part2(input: List[String]): Int = {
    val grid = input.map(_.toCharArray).toArray
    var totalRemoved = 0

    while (true) {
      val toRemove = accessiblePositions(grid)

      if (toRemove.isEmpty)
        return totalRemoved

      // Quitarlos: poner '.'
      toRemove.foreach { case (r, c) => grid(r)(c) = '.' }

      totalRemoved += toRemove.size
    }

    totalRemoved
  }

  def main(args: Array[String]): Unit = {
    val data = Input.readLines(4).map(_.trim).filter(_.nonEmpty)
    println(s"Part 1: ${part1(data)}")
    println(s"Part 2: ${part2(data)}")
  }
}
