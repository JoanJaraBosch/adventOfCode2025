package aoc

import util.Input

object Day11 {

  def countPaths(
                  graph: Map[String, List[String]],
                  start: String
                ): Long = {

    val memo = scala.collection.mutable.Map[String, Long]()

    def dfs(node: String): Long =
      memo.getOrElseUpdate(node, {
        if (node == "out") 1L
        else
          graph
            .getOrElse(node, Nil)
            .map(dfs)
            .sum
      })

    dfs(start)
  }

  def countPathsPart2(
                       graph: Map[String, List[String]],
                       start: String
                     ): Long = {

    val memo = scala.collection.mutable.Map[(String, Boolean, Boolean), Long]()

    def dfs(node: String, seenDac: Boolean, seenFft: Boolean): Long = {
      val newSeenDac = seenDac || node == "dac"
      val newSeenFft = seenFft || node == "fft"

      val key = (node, newSeenDac, newSeenFft)

      memo.getOrElseUpdate(key, {
        if (node == "out") {
          if (newSeenDac && newSeenFft) 1L else 0L
        } else {
          graph
            .getOrElse(node, Nil)
            .map(child => dfs(child, newSeenDac, newSeenFft))
            .sum
        }
      })
    }

    dfs(start, seenDac = false, seenFft = false)
  }

  def part1(input: List[String]): Long = {
    val graph: Map[String, List[String]] =
      input.map { line =>
        val Array(node, rest) = line.split(":").map(_.trim)
        val children = rest.split("\\s+").toList
        node -> children
      }.toMap

    countPaths(graph, "you")
  }

  def part2(input: List[String]): Long = {
    val graph: Map[String, List[String]] =
      input.map { line =>
        val Array(node, rest) = line.split(":").map(_.trim)
        node -> rest.split("\\s+").toList
      }.toMap

    countPathsPart2(graph, "svr")
  }

  def main(args: Array[String]): Unit = {
    val data = Input.readLines(11).map(_.trim).filter(_.nonEmpty)
    println(s"Part 1: ${part1(data)}")
    println(s"Part 2: ${part2(data)}")
  }
}
