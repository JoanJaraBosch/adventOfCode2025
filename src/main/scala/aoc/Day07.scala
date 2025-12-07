package aoc

import util.Input
import scala.collection.mutable

object Day07 {

  private case class Beam(row: Int, col: Int)

  def part1(lines: List[String]): Int = {
    val rows = lines.length
    val cols = lines.head.length
    val grid = lines.map(_.toCharArray).toArray

    // Buscar la Beamición de S
    val startCol = grid(0).indexOf('S')
    var activeBeams = mutable.Set(Beam(0, startCol))
    var splits = 0

    while (activeBeams.nonEmpty) {
      val nextBeams = mutable.Set.empty[Beam]

      for (beam <- activeBeams) {
        val r = beam.row + 1
        val c = beam.col

        if (r < rows) {
          grid(r)(c) match {
            case '.' =>
              nextBeams += Beam(r, c)  // sigue hacia abajo
            case '^' =>
              splits += 1
              if (c > 0) nextBeams += Beam(r, c - 1) // izquierda
              if (c < cols - 1) nextBeams += Beam(r, c + 1) // derecha
            case _ => // no hacer nada
          }
        }
      }

      activeBeams = nextBeams
    }

    splits
  }

  def part2(lines: List[String]): Long = {
    val rows = lines.length
    val cols = lines.head.length
    val grid = lines.map(_.toCharArray).toArray

    // Buscar la posición de S
    val startCol = grid(0).indexOf('S')
    var active = mutable.Map(Beam(0, startCol) -> 1L)
    var totalTimelines: Long = 0

    while (active.nonEmpty) {
      val next = mutable.Map.empty[Beam, Long]

      for ((beam, count) <- active) {
        val r = beam.row + 1
        val c = beam.col

        if (r >= rows) {
          // El haz salió del mapa
          totalTimelines += count
        } else {
          grid(r)(c) match {
            case '.' =>
              val existing = next.getOrElse(Beam(r, c), 0L)
              next(Beam(r, c)) = existing + count
            case '^' =>
              // Cada timeline se divide en dos
              if (c > 0) {
                val existingL = next.getOrElse(Beam(r, c - 1), 0L)
                next(Beam(r, c - 1)) = existingL + count
              }
              if (c < cols - 1) {
                val existingR = next.getOrElse(Beam(r, c + 1), 0L)
                next(Beam(r, c + 1)) = existingR + count
              }
            case _ => // nada
          }
        }
      }

      active = next
    }

    totalTimelines
  }

  def main(args: Array[String]): Unit = {
    val data = Input.readLines(7).map(_.trim).filter(_.nonEmpty)
    println(s"Part 1: ${part1(data)}")
    println(s"Part 2: ${part2(data)}")
  }
}
