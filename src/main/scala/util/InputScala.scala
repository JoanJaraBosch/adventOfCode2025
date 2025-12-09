package util

import scala.io.Source

object InputScala {
  def read(day: Int): String = {
    val filename = f"inputScala/day$day%02d.txt"
    val stream = getClass.getClassLoader.getResourceAsStream(filename)
    Source.fromInputStream(stream).mkString
  }

  def readLines(day: Int): List[String] =
    read(day).split("\n").toList
}
