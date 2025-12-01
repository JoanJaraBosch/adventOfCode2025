package aoc

import util.Input

object Day01 {

  def part1(lines: List[String]): Int = {
    var pos = 50
    var endsOnZero = 0

    for (op <- lines) {
      val dir = op.head
      val d   = op.tail.toInt

      if (dir == 'R') {
        pos = (pos + d) % 100
      } else { // L
        pos = (pos - d) % 100
        if (pos < 0) pos += 100
      }

      if (pos == 0) endsOnZero += 1
    }

    endsOnZero
  }

  def part2(lines: List[String]): Int = {
    var pos = 50
    var clicksOnZero = 0

    for (op <- lines) {
      val dir = op.head
      val d   = op.tail.toInt

      if (dir == 'R') {
        val hits = (pos + d) / 100
        clicksOnZero += hits
        pos = (pos + d) % 100
      } else { // L
        val hits =
          if (pos == 0) {
            d / 100
          } else {
            if (d >= pos) {
              (d - pos) / 100 + 1
            } else {
              0
            }
          }
        clicksOnZero += hits
        pos = (pos - d) % 100
        if (pos < 0) pos += 100
      }
    }

    clicksOnZero
  }

  def main(args: Array[String]): Unit = {
    val data = Input.readLines(1).map(_.trim).filter(_.nonEmpty)
    println("Part 1: " + part1(data))
    println("Part 2: " + part2(data))
  }
}