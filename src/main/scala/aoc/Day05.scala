package aoc

import util.Input

object Day05 {

  case class RangeAB(a: Long, b: Long)

  private def countFresh(elem: String, database: List[String]): Boolean = {
    database.foreach {
      data =>
        if(data.split("-")(0).toLong <= elem.toLong && elem.toLong<= data.split("-")(1).toLong) return true
    }
    false
  }

  private def mapRange(data: String): RangeAB = {
    val Array(a, b) = data.split("-").map(_.toLong)
    RangeAB(a,b)
  }

  def part1(database: List[String], ingredient: List[String]): Int = {
    var count = 0
    ingredient.foreach {
     elem =>
       if(countFresh(elem, database)) count = count + 1
    }
    count
  }

  def part2(database: List[String]): Long = {
    database.map(mapRange).sortBy(_.a).foldLeft(List.empty[RangeAB]) {
      (acc, curr) =>
        acc match {
          case Nil => curr :: Nil
          case head :: tail =>
            if (curr.a <= head.b + 1) {
              RangeAB(head.a, math.max(head.b, curr.b)) :: tail
            } else {
              curr :: acc
            }
        }
    }.map(r => r.b - r.a + 1).sum
  }

  def main(args: Array[String]): Unit = {
    val data = Input.readLines(5).map(_.trim).filter(_.nonEmpty)
    val (database, ingredient) = data.partition(_.contains("-"))
    println(s"Part 1: ${part1(database, ingredient)}")
    println(s"Part 2: ${part2(database)}")
  }
}
