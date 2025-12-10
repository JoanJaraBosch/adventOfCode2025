package aoc

import util.Input
import scala.collection.mutable
import com.google.ortools.Loader
import com.google.ortools.sat._

object Day10 {

  private case class Machine(goal: Int, buttons: Array[Int])
  private case class Machine2(goal: Array[Int], buttons: Array[Array[Int]])

  private def parseLine(line: String): Machine = {
    // Goal: [.#.#..]
    val goalStr = line.substring(line.indexOf('[') + 1, line.indexOf(']'))
    val n = goalStr.length
    var goal = 0
    for (i <- 0 until n) {
      if (goalStr(i) == '#') goal |= (1 << i)
    }

    // Buttons: (0,1,3) ...
    val buttonParts = line.split("\\(").drop(1)
    val buttons = mutable.ArrayBuffer[Int]()
    for (p <- buttonParts) {
      val idx = p.indexOf(")")
      if (idx > 0) {
        val content = p.substring(0, idx).trim
        if (content.nonEmpty) {
          val nums = content.split(",").map(_.trim.toInt)
          var mask = 0
          for (k <- nums) mask |= (1 << k)
          buttons += mask
        }
      }
    }

    Machine(goal, buttons.toArray)
  }

  private def parseLine2(line: String): Machine2 = {
    // Extraer los {} de joltage goals
    val start = line.indexOf('{')
    val end = line.indexOf('}')
    val goalStr = line.substring(start + 1, end)
    val goal = goalStr.split(",").map(_.trim.toInt)

    // Extraer los botones (mismos que en parte 1)
    val buttonParts = line.split("\\(").drop(1)
    val buttons = mutable.ArrayBuffer[Array[Int]]()
    for (p <- buttonParts) {
      val idx = p.indexOf(")")
      if (idx > 0) {
        val content = p.substring(0, idx).trim
        if (content.nonEmpty) {
          val nums = content.split(",").map(_.trim.toInt)
          // construimos vector como 0/1
          val vec = Array.fill(goal.length)(0)
          for (i <- nums) vec(i) = 1
          buttons += vec
        }
      }
    }

    Machine2(goal, buttons.toArray)
  }

  private def solveMachine(m: Machine): Int = {
    val buttons = m.buttons
    val goal = m.goal
    val dist = mutable.Map[Int, Int]()
    val q = mutable.Queue[Int]()

    dist(0) = 0
    q.enqueue(0)

    while (q.nonEmpty) {
      val cur = q.dequeue()
      val d = dist(cur)

      if (cur == goal) return d

      for (b <- buttons) {
        val nxt = cur ^ b
        if (!dist.contains(nxt)) {
          dist(nxt) = d + 1
          q.enqueue(nxt)
        }
      }
    }

    sys.error("No solution? Should never happen.")
  }

  def solveMachine2(buttons: List[Array[Int]], target: Array[Int]): Long = {
    Loader.loadNativeLibraries()

    val model = new CpModel()

    // Creamos una variable de número de pulsaciones para cada botón
    val press: Array[IntVar] = buttons.indices
      .map(i => model.newIntVar(0, 1_000_000, s"p$i"))
      .toArray

    // Para cada contador (target)
    for (j <- target.indices) {
      // Botones que afectan este contador
      val involved = buttons.indices.filter(i => buttons(i)(j) == 1)

      if (involved.nonEmpty) {
        val vars: Array[IntVar] = involved.map(press).toArray
        val linVars: Array[LinearArgument] = vars.map(v => v: LinearArgument)
        val coeffs: Array[Long] = Array.fill(vars.length)(1L)
        model.addEquality(LinearExpr.weightedSum(linVars, coeffs), target(j))
      } else if (target(j) != 0) {
        // Ningún botón puede incrementar este contador, y target != 0 → imposible
        throw new IllegalArgumentException(s"No buttons can reach counter $j but target is ${target(j)}")
      }
    }

    // Minimizar la suma de pulsaciones
    val objective = LinearExpr.sum(press.map(v => v: LinearArgument))
    model.minimize(objective)

    val solver = new CpSolver()
    val status = solver.solve(model)

    if (status != CpSolverStatus.OPTIMAL && status != CpSolverStatus.FEASIBLE) {
      throw new RuntimeException("Solver no encontró solución")
    }

    // Sumar las pulsaciones obtenidas
    press.map(p => solver.value(p)).sum
  }

  def part1(input: List[String]): Int = {
    input.filter(_.nonEmpty).map(parseLine).map(solveMachine).sum
  }

  def part2(input: List[String]): Long = {
    input.filter(_.nonEmpty).map { line =>
      val m = parseLine2(line)
      val buttonsList: List[Array[Int]] = m.buttons.toList // Array[Array[Int]] -> List[Array[Int]]
      solveMachine2(buttonsList, m.goal)
    }.map(_.toLong).sum // fuerza a Long para evitar conflicto de implicits
  }

  def main(args: Array[String]): Unit = {
    val data = Input.readLines(10).map(_.trim).filter(_.nonEmpty)
    println(s"Part 1: ${part1(data)}")
    println(s"Part 2: ${part2(data)}")
  }
}
