package aocscala

object Runner {

  val days: Map[Int, () => Unit] = Map(
    1 -> (() => Day01.main(Array())),
    2 -> (() => Day02.main(Array())),
    3 -> (() => Day03.main(Array())),
    4 -> (() => Day04.main(Array())),
    5 -> (() => Day05.main(Array())),
    6 -> (() => Day06.main(Array())),
    7 -> (() => Day07.main(Array())),
    8 -> (() => Day08.main(Array())),
    9 -> (() => Day09.main(Array())),
    10 -> (() => Day10.main(Array())),
    11 -> (() => Day11.main(Array())),
    12 -> (() => Day12.main(Array())),
    13 -> (() => Day13.main(Array())),
    14 -> (() => Day14.main(Array())),
    15 -> (() => Day15.main(Array())),
    16 -> (() => Day16.main(Array())),
    17 -> (() => Day17.main(Array())),
    18 -> (() => Day18.main(Array())),
    19 -> (() => Day19.main(Array())),
    20 -> (() => Day20.main(Array())),
    21 -> (() => Day21.main(Array())),
    22 -> (() => Day22.main(Array())),
    23 -> (() => Day23.main(Array())),
    24 -> (() => Day24.main(Array())),
    25 -> (() => Day25.main(Array())),
  )

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Usage: run <day>")
      return
    }

    val day = args(0).toInt

    days.get(day) match {
      case Some(runDay) => runDay()
      case None => println(s"Day $day not implemented")
    }
  }
}
