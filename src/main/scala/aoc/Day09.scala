package aoc

import util.Input
import scala.collection.mutable

object Day09 {

  private case class Point(x: Int, y: Int)

  private def maxRectangleArea(lines: List[String]): Long = {
    val points = lines
      .map(_.trim)
      .filter(_.nonEmpty)
      .map { line =>
        val arr = line.split(",").map(_.trim)
        Point(arr(0).toInt, arr(1).toInt)
      }

    // quick guard 4757983825 4758121828
    if (points.size < 2) return 0L

    var maxArea: Long = 0L

    for {
      (p1, i) <- points.zipWithIndex
      p2 <- points.drop(i + 1)
    } {
      val dx = Math.abs(p1.x - p2.x).toLong + 1L
      val dy = Math.abs(p1.y - p2.y).toLong + 1L
      val area = dx * dy
      if (area > maxArea) maxArea = area
    }

    maxArea
  }

  private def buildRealGrid(points: Array[Point]): Array[Array[Boolean]] = {
    // Comprimir coordenadas
    val xset = mutable.Set[Int]()
    val yset = mutable.Set[Int]()
    for (p <- points) {
      xset += p.x
      xset += p.x + 1
      yset += p.y
      yset += p.y + 1
    }
    val xlst = xset.toArray.sorted
    val ylst = yset.toArray.sorted
    val xmap = xlst.zipWithIndex.toMap
    val ymap = ylst.zipWithIndex.toMap

    val W = xlst.length
    val H = ylst.length
    val grid = Array.fill(W, H)(0)

    // marcar bordes como en Python
    for (i <- points.indices) {
      val p1 = points(i)
      val p2 = points((i + 1) % points.length)
      var x1 = xmap(p1.x)
      var y1 = ymap(p1.y)
      var x2 = xmap(p2.x)
      var y2 = ymap(p2.y)

      if (x1 != x2) {
        assert(y1 == y2)
        if (x1 > x2) { val tmp = x1; x1 = x2; x2 = tmp }
        grid(x1)(y1) |= 1
        grid(x2)(y1) |= 2
        for (x <- x1 + 1 until x2) grid(x)(y1) |= 3
      } else if (y1 != y2) {
        assert(x1 == x2)
        if (y1 > y2) { val tmp = y1; y1 = y2; y2 = tmp }
        grid(x1)(y1) |= 1
        grid(x1)(y2) |= 2
        for (y <- y1 + 1 until y2) grid(x1)(y) |= 3
      }
    }

    // scanline horizontal para marcar tiles rojos y verdes
    val realgrid = Array.ofDim[Boolean](W, H)
    for (i <- 0 until W) {
      var nxt = 0
      for (j <- 0 until H) {
        realgrid(i)(j) = (nxt > 0) || ((grid(i)(j) & 3) > 0)
        nxt ^= grid(i)(j) & 3
      }
    }

    // rellenar internals verticales (flood fill simplificado)
    // cualquier columna donde un tile verde esté entre bordes rojos se llena
    for (j <- 0 until H) {
      var inside = false
      for (i <- 0 until W) {
        if ((grid(i)(j) & 3) > 0) inside = !inside
        if (inside) realgrid(i)(j) = true
      }
    }

    realgrid
  }

  private def computePrefixSum(realgrid: Array[Array[Boolean]]): Array[Array[Int]] = {
    val W = realgrid.length
    val H = realgrid(0).length
    val sums = Array.ofDim[Int](W + 1, H + 1)
    for (i <- 0 until W; j <- 0 until H) {
      sums(i + 1)(j + 1) = (if (realgrid(i)(j)) 1 else 0) + sums(i)(j + 1) + sums(i + 1)(j) - sums(i)(j)
    }
    sums
  }

  def part1(input: List[String]): Long = maxRectangleArea(input)

  def part2(lines: List[String]): Long = {
    val points = lines.filter(_.nonEmpty).map { s =>
      val a = s.split(",").map(_.trim.toInt)
      Point(a(0), a(1))
    }.toArray

    val realgrid = buildRealGrid(points)

    // reconstruir mapas de coordenadas
    val xset = mutable.Set[Int]()
    val yset = mutable.Set[Int]()
    for (p <- points) { xset += p.x; xset += p.x + 1; yset += p.y; yset += p.y + 1 }
    val xlst = xset.toArray.sorted
    val ylst = yset.toArray.sorted
    val xmap = xlst.zipWithIndex.toMap
    val ymap = ylst.zipWithIndex.toMap

    val sums = computePrefixSum(realgrid)

    // verificar todos los pares de puntos rojos
    var maxArea = 0L
    for (i <- points.indices) {
      val p1 = points(i)
      for (j <- 0 until i) {
        val p2 = points(j)
        if (p1.x != p2.x && p1.y != p2.y) { // diagonales válidas
          val x1 = math.min(p1.x, p2.x)
          val x2 = math.max(p1.x, p2.x)
          val y1 = math.min(p1.y, p2.y)
          val y2 = math.max(p1.y, p2.y)
          val xr1 = xmap(x1)
          val xr2 = xmap(x2)
          val yr1 = ymap(y1)
          val yr2 = ymap(y2)
          val numGood = sums(xr2 + 1)(yr2 + 1) - sums(xr2 + 1)(yr1) - sums(xr1)(yr2 + 1) + sums(xr1)(yr1)
          if (numGood == (xr2 - xr1 + 1) * (yr2 - yr1 + 1)) {
            maxArea = math.max(maxArea, (x2 - x1 + 1L) * (y2 - y1 + 1L))
          }
        }
      }
    }

    maxArea
  }

  def main(args: Array[String]): Unit = {
    val data = Input.readLines(9).map(_.trim).filter(_.nonEmpty)
    println(s"Part 1: ${part1(data)}")
    println(s"Part 2: ${part2(data)}")
  }
}
