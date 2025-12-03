package aoc

import util.Input

object Day03 {

  private def maxPart1(elem: String): Int = {
    val digits = elem.map(_.asDigit)

    (for {
      i <- digits.indices
      j <- (i + 1) until digits.length
    } yield digits(i) * 10 + digits(j)).max
  }

  private def maxPart2(elem: String): BigInt = {
    val n = elem.length
    require(12 >= 0 && 12 <= n, s"k (12) debe estar entre 0 y longitud ($n)")

    val sb = new StringBuilder
    var start = 0

    for (pos <- 0 until 12) {
      // límite inclusive donde podemos buscar el dígito para la posición pos
      val maxIndexAllowed = n - (12 - pos)
      // buscamos el dígito máximo en elem[start .. maxIndexAllowed]
      var bestDigit = -1
      var bestIdx = start
      var i = start
      while (i <= maxIndexAllowed) {
        val d = elem.charAt(i) - '0'
        if (d > bestDigit) {
          bestDigit = d
          bestIdx = i
          if (bestDigit == 9) { // atajo: no puede mejorar
            // avanzar i fuera del bucle
            i = maxIndexAllowed + 1
          } else i += 1
        } else i += 1
      }
      sb.append((bestDigit + '0').toChar)
      start = bestIdx + 1
    }

    BigInt(sb.toString())
  }

  def part1(input: List[String]): Int = {
    input.map(maxPart1).sum
  }

  def part2(input: List[String]): BigInt = {
    input.map(line => maxPart2(line)).foldLeft(BigInt(0))(_ + _)
  }

  def main(args: Array[String]): Unit = {
    val data = Input.readLines(3).map(_.trim).filter(_.nonEmpty)
    println(s"Part 1: ${part1(data)}")
    println(s"Part 2: ${part2(data)}")
  }
}
