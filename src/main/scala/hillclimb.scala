package experiment
import java.util.UUID.randomUUID
import System._

object Hillclimb { // extends App {

  def cutoff(maxTime: Long, maxIter: Int, timeOrIterations: String = "time")
    (curIter: Int, startTime: Long): Boolean =
    if (timeOrIterations == "time") nanoTime - startTime > maxTime
    else                            curIter > maxIter

  def hillclimb(
    index: Int,
    graphSize: Int,
    maxEvaluations: Int,
    folderName: String,
    graph: Array[Array[Int]],
    totalEvaluations: Int,
    algorithm: Solver
  ): Int = {

    val folder = randomUUID
    var changed = true
    var currentGraph = graph
    val temp = algorithm.solve(currentGraph, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
    var maxFitness = temp._2
    var maxHamiltonian = true
    var bestPath = List[Int]()
    var i = 0

    temp._1 match {
      case Some(tempHam) => {
        temp._4 match {
          case Some(tempSol) => {
            val json = GraphGenerator.graphToJson(-1, currentGraph, temp._2, tempHam, tempSol :+ tempSol.head)
            GraphGenerator.writeGraphToFile(s"results/$folderName/hillclimb/$graphSize/$totalEvaluations/$index", -1, json)
          }
          case None => {
            val json = GraphGenerator.graphToJson(-1, currentGraph, temp._2, tempHam, List())
            GraphGenerator.writeGraphToFile(s"results/$folderName/hillclimb/$graphSize/$totalEvaluations/$index", -1, json)
          }
        }
      }
      case None => {}
    }

    while (i < maxEvaluations) {
      val candidate = Utils.randomMutation(currentGraph)
      val (hamiltonian, recursions, time, path) = algorithm.solve(candidate, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
      
      if (recursions >= maxFitness) {
        changed = true
        currentGraph = candidate
        maxFitness = recursions
        path match {
          case Some(temp) => { bestPath = temp }
          case None => { bestPath = List()}
        }
        hamiltonian match {
          case Some(true) => { maxHamiltonian = true}
          case Some(false) => { maxHamiltonian = false}
          case None => { maxHamiltonian = false}
        }
        val bp = if (bestPath.isEmpty) Nil else bestPath :+ bestPath.head
        val json = GraphGenerator.graphToJson(i, currentGraph, recursions, maxHamiltonian, bp)
        GraphGenerator.writeGraphToFile(s"results/$folderName/hillclimb/$graphSize/$totalEvaluations/$index", i, json)
      }
      i = i + 1
    }
    return maxFitness
  }
}
