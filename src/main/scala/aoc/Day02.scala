package aoc

import util.Input

object Day02 {

  private def isInvalidIDPart1(n: Long): Boolean = {
    val s = n.toString
    val len = s.length

    // Solo tamaños pares pueden ser doble repetición
    if (len % 2 != 0) return false

    val half = len / 2
    val left = s.substring(0, half)
    val right = s.substring(half)

    left == right
  }

  private def isInvalidIDPart2(n: Long): Boolean = {
    val s = n.toString

    // longitud mínima de la secuencia: 1
    val len = s.length

    // probamos todos los tamaños posibles de bloque
    for (blockLen <- 1 to len / 2) {
      if (len % blockLen == 0) {           // solo bloques que dividen exactamente el número
        val block = s.substring(0, blockLen)
        val times = len / blockLen
        if (block * times == s) return true
      }
    }

    false
  }


  def part1(input: List[String]): Long = {
    var suma: Long = 0
    input.foreach {elem =>
      val Array(a, b) = elem.split("-").map(_.toLong)
      suma = suma + (a to b).toList.filter(isInvalidIDPart1).sum // Range inclusive
    }
    suma
  }

  def part2(input: List[String]): Long = {
    var suma: Long = 0
    input.foreach {elem =>
      val Array(a, b) = elem.split("-").map(_.toLong)
      suma = suma + (a to b).toList.filter(isInvalidIDPart2).sum // Range inclusive
    }
    suma
  }

  def main(args: Array[String]): Unit = {
    val data = Input.readLines(2).map(_.trim).filter(_.nonEmpty)
    val array_data= data.flatMap(_.split(","))
    println(s"Part 1: ${part1(array_data)}")
    println(s"Part 2: ${part2(array_data)}")
  }
}
