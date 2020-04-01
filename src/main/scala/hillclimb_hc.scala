package experiment
import java.util.UUID.randomUUID
import System._


object HillclimbHC { // extends App {

  def cutoff(maxTime: Long, maxIter: Int, timeOrIterations: String = "time")
    (curIter: Int, startTime: Long): Boolean =
    if (timeOrIterations == "time") nanoTime - startTime > maxTime
    else                            curIter > maxIter

  def hillclimb(index: Int, graphSize: Int, maxEvaluations: Int, folderName: String, graph: Array[Array[Int]], totalEvaluations: Int): Int = {
    val folder = randomUUID
    var changed = true
    var currentGraph = graph
    val temp = CheckAllWithPruningLow.solve(currentGraph, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
    var maxFitness = temp._2
    var maxHamiltonian = true
    var bestPath = List[Int]()
    var i = 0
    // val directory = new Directory(new File(s"results/hillclimb-hc/$maxEvaluations-evaluations/$graphSize-size/$index"))
    // Utils.delete(s"results/hillclimb-hc/$maxEvaluations-evaluations/$graphSize-size/$index")
    // try {
    //   path.deleteRecursively(continueOnFailure = false) 
    // } catch {
    //   case e: IOException => // some file could not be deleted
    // }
    // directory.deleteRecursively()

    while (i < maxEvaluations) {
      val candidate = Utils.randomMutationHC(currentGraph)
      val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(candidate, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
      if (recursions >= maxFitness) {
        changed = true
        currentGraph = candidate
        maxFitness = recursions
        path match {
          case Some(temp) => { bestPath = temp }
          case None => {}
        }
        hamiltonian match {
          case Some(true) => { maxHamiltonian = true}
          case Some(false) => { maxHamiltonian = false}
          case None => { maxHamiltonian = false}
        }
        val json = GraphGenerator.graphToJson(i, candidate, recursions, maxHamiltonian, bestPath :+ bestPath.head)
        GraphGenerator.writeGraphToFile(s"results/$folderName/hillclimb-hc/$graphSize/$totalEvaluations/$index", i, json)
      }
      i = i + 1
    }
    return maxFitness
  }

  // val maxEvaluations = 2000
  // val numberOfGraphs = 10
  // val graphSizes = List(12) //, 14) // , 16, 18, 20)

  // def sum(i: Array[Array[Int]]): Int = {
  //   var result = 0
  //   i.foreach(row => {
  //     row.foreach(result += _)
  //   })
  //   result
  // }

  // for (graphSize <- graphSizes) {
  //   println(s"Handeling graph size: $graphSize")
  //   val numberOfEdges = ((scala.math.log(graphSize) + scala.math.log(scala.math.log(graphSize))) * graphSize / 2).toInt

  //   for (i <- 1 until numberOfGraphs) {
  //     val startingGraph = GraphGenerator.genGraphWithHamiltonCycle(graphSize, numberOfEdges)
  //     val (graph, fitness, hamiltonian, bestPath) = hillclimb(i, graphSize, maxEvaluations, startingGraph)
  //     // GraphGenerator.printGraph(graph)
  //     println(s"Graph generated: $fitness")
  //     // println(CheckAllWithPruningLow.solve(graph, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations")))
  //     // println(CheckOneDegreeWithPruning.solve(graph, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations")))
  //     // println("sum", sum(graph))
  //     val json = GraphGenerator.graphToJson(10, graph, fitness, hamiltonian, bestPath :+ bestPath.head)
  //     GraphGenerator.writeGraphToFile(s"super_results", 10, json)
  //   }
  // }
}
