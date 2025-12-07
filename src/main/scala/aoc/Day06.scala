package aoc

import util.Input

object Day06 {

  def part1(operands: List[String],numbers: List[List[String]]): Long = {
    var cont = 0
    var result: Long =0
    var loopProd: Long = 1
    var loopSuma: Long = 0

    operands.foreach{
      operand =>
        numbers.foreach{
          list =>
            if(operand == "*") loopProd = loopProd * list(cont).toLong
            else loopSuma = loopSuma + list(cont).toLong
        }
        if(loopSuma == 0 && loopProd != 1) result = result + loopProd
        else result = result + loopSuma

        loopProd = 1
        loopSuma = 0
        cont = cont + 1
    }

    result
  }

  def part2(lines: List[String]): BigInt = {
    //No ho entenia, la part2 es copiada
    0
  }

  def evaluate(expr: List[String]): Long = {
    val operands = expr.init.map(_.toLong)
    expr.last match{
      case "+" => operands.sum
      case "*" => operands.product
    }
  }

  def partOne(input: List[String]): Long = {
    input.map(_.trim.split(" +")).transpose.map(evaluate).sum
  }

  def partTwo(input: List[String]): Long = {
    def groupColumns(cols: List[List[Char]]): List[List[List[Char]]] = {
      if(cols.isEmpty) Nil
      else {
        val (group, rest) = cols.span(_.exists(_.isDigit))
        group :: groupColumns (rest.drop (1) )
      }

    }
    def processGroup(group: List[List[Char]]): List[String] = {
      val numbers = group.map(_.init.mkString.trim)
      val operator = group.head.last.toString
      numbers :+ operator
    }
      val maxLen = input.map(_.length).max
      val cols = input.map(_.padTo(maxLen, ' ').toList).transpose
      groupColumns(cols).map(processGroup).map(evaluate).sum
  }

  def main(args: Array[String]): Unit = {
    val raw = Input.readLines(6).toList
    val data = raw.map(_.trim).filter(_.nonEmpty)
    val operands = data.last.split("\\s+").toList

    val numbers = data.init.map(_.split("\\s+").toList)
    println(s"Part 1: ${part1(operands, numbers)}")
    println(s"Part 1 optimized: ${partOne(raw)}")
    println(s"Part 2: ${part2(raw)}")
    //https://github.com/lupari/aoc2025/blob/main/src/main/scala/assignments/Day06.scala
    println(s"Part 2 credits to lupari: ${partTwo(raw)}")
  }
}
