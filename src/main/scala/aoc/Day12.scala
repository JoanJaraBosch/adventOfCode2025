package aoc

import util.{Complex, DFS, Grid, Problem, Files}

case class Day12(input: Vector[String]) extends Problem {
  override def solve1(): Unit = {
    val (tiles, instructions) = parse()

    val allTilePermutations = tiles.map {
      tile => Set(
        tile,
        tile.rotateRight(),
        tile.rotateRight().rotateRight(),
        tile.rotateRight().rotateRight().rotateRight(),
        tile.flipVertically(),
        tile.flipVertically().rotateRight(),
        tile.flipVertically().rotateRight().rotateRight(),
        tile.flipVertically().rotateRight().rotateRight().rotateRight(),
      ).toVector
    }.zipWithIndex

    val result = instructions.count {
      case Instruction(width, height, requirements) =>
        val area = width * height
        val areaInTiles = (width / 3) * (height / 3)
        val totalTiles = requirements.sum

        val requiredArea = requirements.zip(tiles).map { case (requirement, tile) =>
          requirement * tile.count(_ == '#')
        }.sum

        if (areaInTiles >= totalTiles) true
        else if (requiredArea >= area) false
        else {
          // The below very slowly finds a real placement
          // In practice, this branch doesn't get triggered for a real input
          val grid = Grid.of(height, width, '.')
          val initialState = (grid, requirements)

          DFS.solve(initialState)(state => state._2.forall(_ == 0)) { case ((grid, requirements), _) =>
            allTilePermutations.flatMap { permutations =>
              permutations._1.flatMap { tile =>
                if (requirements(permutations._2) == 0) Seq[(Grid[Char], Vector[Int])]()
                else {
                  val possiblePlacements = grid.zipWithIndex.filter { case (c, address) =>
                    if (-(address + Complex(0, -tile.columnLength)).im > grid.columnLength) false
                    else if ((address + Complex(tile.rowLength, 0)).re > grid.rowLength) false
                    else {
                      val slice = grid.slice(address, tile.rowLength, tile.columnLength)
                      slice.zipWithIndex.forall { case (s, sliceAddress) =>
                        if (tile(sliceAddress) == '#') s == '.'
                        else true
                      }
                    }
                  }.map(_._2)

                  val updatedGrids = possiblePlacements.map { address =>
                    grid.updated(address, tile)
                  }

                  updatedGrids.map { grid =>
                    (grid, requirements.updated(permutations._2, requirements(permutations._2) - 1))
                  }
                }
              }
            }
          }.isDefined
        }
    }

    println(s"Result 1: $result")
  }

  override def solve2(): Unit = {
    println("Finish decorating!")
  }

  private def parse(): (Vector[Grid[Char]], Vector[Instruction]) = {
    val (tiles, instructionLines) = input.span(!_.contains("x"))

    val tileArray = tiles.mkString("\n").trim.split("\n\n")

    val tileGrids = tileArray.map { tileString =>
      Grid(tileString.split("\n").tail.map(_.toVector).toVector)
    }.toVector

    val pattern = "(\\d+)x(\\d+): (.*?)".r

    val instructions = instructionLines.map {
      case pattern(width, height, requirementString) =>
        val requirements = requirementString.trim.split(" ").map(_.toInt).toVector

        Instruction(width.toInt, height.toInt, requirements)
    }

    (tileGrids, instructions)
  }

  private case class Instruction(width: Int, height: Int, requirements: Vector[Int])
}

case object Day12 extends App {
  val input = Files.lines("input/day12.txt")
  val problem = Day12(input)
  problem.solve1()
  problem.solve2()
}