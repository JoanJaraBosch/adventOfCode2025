package util

import scala.io.Source

object Files {
  def lines(filename: String): Vector[String] = {
    Source.fromResource(filename).getLines().toVector
  }
}