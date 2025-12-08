package aoc

import util.Input

import scala.collection.mutable.ListBuffer

object Day08 {

  private case class Box(x: Double, y: Double, z: Double)
  private case class Distance(boxA: Box, boxB: Box, distance: Double)

  def dist3D(boxA: Box, boxB: Box): Double = {
    math.sqrt(
      math.pow(boxA.x - boxB.x, 2) +
        math.pow(boxA.y - boxB.y, 2) +
        math.pow(boxA.z - boxB.z, 2)
    )
  }

  private def boxDistances(boxA: Box, boxB: Box): Distance = {
    Distance(boxA, boxB, dist3D(boxA, boxB))
  }

  def part1(input: List[String]): Long = {
    val listBuffer = new ListBuffer[Box]
    input.foreach {
      elem =>
        val elemDiv = elem.split(",")
        listBuffer.append(Box(elemDiv(0).toDouble, elemDiv(1).toDouble, elemDiv(2).toDouble))
    }

    val boxes = listBuffer.toList
    val distances =
      for {
        (boxA, i) <- boxes.zipWithIndex
        boxB <- boxes.drop(i + 1)
      } yield {
        boxDistances(boxA, boxB)
      }
    val sortedDist = ListBuffer(distances.sortBy(_.distance).take(1000): _*)
    val circuits = ListBuffer[ListBuffer[Box]]()

    while (sortedDist.nonEmpty) {
      val elem = sortedDist.remove(0) // quita el primer elemento
      if (circuits.isEmpty) {
        val newCircuit = ListBuffer(elem.boxA, elem.boxB)
        circuits.append(newCircuit)
      } else {
        val circuitA = circuits.find(_.contains(elem.boxA))
        val circuitB = circuits.find(_.contains(elem.boxB))

        (circuitA, circuitB) match {
          case (None, None) =>
            circuits.append(ListBuffer(elem.boxA, elem.boxB))

          case (Some(cA), None) =>
            cA.append(elem.boxB)

          case (None, Some(cB)) =>
            cB.append(elem.boxA)

          case (Some(cA), Some(cB)) =>
            if (cA != cB) {
              cA ++= cB
              circuits -= cB
            }
        }
      }
    }
    val sortedSizes = circuits.map(_.size.toLong).sorted(Ordering[Long].reverse)
    sortedSizes.take(3).product
  }

  def part2(input: List[String]): Long = {
    val boxes = input.map { line =>
      val arr = line.split(",")
      Box(arr(0).toDouble, arr(1).toDouble, arr(2).toDouble)
    }

    val distances =
      for {
        (boxA, i) <- boxes.zipWithIndex
        boxB <- boxes.drop(i + 1)
      } yield Distance(boxA, boxB, dist3D(boxA, boxB))

    val sortedDist = distances.sortBy(_.distance)

    val circuits = scala.collection.mutable.ListBuffer[scala.collection.mutable.Set[Box]]()
    boxes.foreach(b => circuits.append(scala.collection.mutable.Set(b)))

    var lastPair: (Box, Box) = (boxes.head, boxes(1))

    for (d <- sortedDist) {
      val circuitA = circuits.find(_.contains(d.boxA))
      val circuitB = circuits.find(_.contains(d.boxB))

      if (circuitA.isDefined && circuitB.isDefined && circuitA.get != circuitB.get) {
        // unir los dos circuitos
        circuitA.get ++= circuitB.get
        circuits -= circuitB.get
        lastPair = (d.boxA, d.boxB)
      }

      // detener si todos los boxes están en un solo circuito
      if (circuits.size == 1) {
        return (lastPair._1.x * lastPair._2.x).toLong
      }
    }

    0L // fallback
  }

  def main(args: Array[String]): Unit = {
    val data = Input.readLines(8).map(_.trim).filter(_.nonEmpty)
    println(s"Part 1: ${part1(data)}")
    println(s"Part 2: ${part2(data)}")
  }
}
