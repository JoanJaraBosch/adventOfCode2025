package util

import scala.io.Source

object Input {
  def read(day: Int): String = {
    val filename = f"input/day$day%02d.txt"
    val stream = getClass.getClassLoader.getResourceAsStream(filename)
    Source.fromInputStream(stream).mkString
  }

  def readLines(day: Int): List[String] =
    read(day).split("\n").toList
}
