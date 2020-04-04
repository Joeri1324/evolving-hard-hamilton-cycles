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
    // Utils.delete(s"results/hillclimb/$maxEvaluations-evaluations/$graphSize-size/$index")

    
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

  // val maxEvaluations = 1000
  // val numberOfGraphs = 10
  // val graphSizes = List(14) //, 16, 18, 20)

  // for (graphSize <- graphSizes) {
  //   println(s"Handeling graph size: $graphSize")
  //   val numberOfEdges = ((scala.math.log(graphSize) + scala.math.log(scala.math.log(graphSize))) * graphSize / 2).toInt

  //   for (i <- 1 until numberOfGraphs) {
  //     val startingGraph = GraphGenerator.genGraph(graphSize, numberOfEdges)
  //     val (graph, fitness, hamiltonian, bestPath) = hillclimb(i, graphSize, maxEvaluations, startingGraph)
  //     println(s"Graph generated: $fitness")
  //   }
  // }
}
