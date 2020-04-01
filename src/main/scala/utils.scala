package experiment
import java.io.File

object Utils {

  def loadPopulation(hamilton: Boolean, index: Int, graphSize: Int, algorithmName: String, evaluations: Int, folderName: String, populationSize: Int, mabyCurrentEval: Option[Int]): (Seq[Array[Array[Int]]], Int) = {
    val folder = s"results/$folderName/$algorithmName/$graphSize"
    val r = scala.util.Random
    val d = new File(folder)

    mabyCurrentEval match {
      case Some(currentEval) => {
        val popSize = new File(s"$folder/$currentEval/$index").listFiles.toList.length

        val graphFiles = { for (i <- 0 until popSize) yield (new File(s"$folder/$currentEval/$index/$i")).listFiles.toList }


        val maxFile = graphFiles(0).map(f => f.toString.split("/").last.stripSuffix(".json").toInt).max

        val population = for (
          i <- 0 until popSize
        ) yield GraphReader.handleFile(s"$folder/$currentEval/$index/$i/$maxFile.json").array

        return (population, currentEval + evaluations)
      }
      case None => {
        if (hamilton) {
          val numberOfEdges = r.nextInt(graphSize * (graphSize - 1) / 2)
          val population = for (_ <- 0 until populationSize) yield GraphGenerator.genGraphWithHamiltonCycle(graphSize, numberOfEdges)
          return (population, evaluations)
        }
        else {
          val numberOfEdges = r.nextInt(graphSize * (graphSize - 1) / 2)
          val population = for (_ <- 0 until populationSize) yield GraphGenerator.genGraph(graphSize, numberOfEdges)
          return (population, evaluations)
        }
      }
    }
    // if (currentEval != None) {
    //   val popSize = new File(s"$folder/$currentEval/$index").listFiles.toList.length

    //   val graphFiles = { for (i <- 0 until popSize) yield (new File(s"$folder/$currentEval/$index/$i")).listFiles.toList }


    //   val maxFile = graphFiles(0).map(f => f.toString.split("/").last.stripSuffix(".json").toInt).max

    //   val population = for (
    //     i <- 0 until popSize
    //    ) yield GraphReader.handleFile(s"$folder/$currentEval/$index/$i/$maxFile.json").array

    //   return (population, currentEval + evaluations)
    // }
    // else if (hamilton) {
    //   val numberOfEdges = r.nextInt(graphSize * (graphSize - 1) / 2)
    //   val population = for (_ <- 0 until populationSize) yield GraphGenerator.genGraphWithHamiltonCycle(graphSize, numberOfEdges)
    //   return (population, evaluations)
    // }
    // else {
    //   val numberOfEdges = r.nextInt(graphSize * (graphSize - 1) / 2)
    //   val population = for (_ <- 0 until populationSize) yield GraphGenerator.genGraph(graphSize, numberOfEdges)
    //   return (population, evaluations)
    // }
  }

  def loadGraph(hamilton: Boolean, index: Int, graphSize: Int, evaluations: Int, folderName: String, algorithmName: String, mabyCurrentEval: Option[Int]): (Array[Array[Int]], Int) = {
    val folder = s"results/$folderName/$algorithmName/$graphSize"
    val r = scala.util.Random
    val numberOfEdges = r.nextInt(graphSize * (graphSize - 1) / 2)
    val d = new File(folder)

    mabyCurrentEval match {
      case Some(currentEval) => {
        val graphFiles = (new File(s"$folder/$currentEval/$index")).listFiles.toList
        val maxFile = graphFiles.map(f => f.toString.split("/").last.stripSuffix(".json").toInt).max
        return (GraphReader.handleFile(s"$folder/$currentEval/$index/$maxFile.json").array, currentEval + evaluations)
      }
      case None => {
        if (hamilton) {
          return (GraphGenerator.genGraphWithHamiltonCycle(graphSize, numberOfEdges), evaluations)
        }
        else {
          return (GraphGenerator.genGraph(graphSize, numberOfEdges), evaluations)
        }
      }
    }
    // if (d.exists) {
    //   val files = d.listFiles.toList
    //   val maxEvaluations = files.map(f => f.toString.split("/").last.toInt).max
    //   val graphFiles = (new File(s"$folder/$maxEvaluations/$index")).listFiles.toList
    //   val maxFile = graphFiles.map(f => f.toString.split("/").last.stripSuffix(".json").toInt).max
    //   return (GraphReader.handleFile(s"$folder/$maxEvaluations/$index/$maxFile.json").array, maxEvaluations + evaluations)
    // }
    // else if (hamilton) {
    //   return (GraphGenerator.genGraphWithHamiltonCycle(graphSize, numberOfEdges), evaluations)
    // }
    // else {
    //   return (GraphGenerator.genGraph(graphSize, numberOfEdges), evaluations)
    // }
  }

  def copy(graph: Array[Array[Int]]): Array[Array[Int]] = {
    return graph.map(_.clone)
  }

  def delete(fileName: String) {
    def rec(file: File): Unit = {
      if (file.isDirectory) 
        Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(rec(_))
      file.delete
    }
    
    val file = new File(fileName)
    rec(file)
  }

  def randomMutations(
    mutations: Int,
    graph: Array[Array[Int]],
    mutationFunction: Array[Array[Int]] => Array[Array[Int]]
  ): Array[Array[Int]] =
    if (mutations == 0) graph
    else                randomMutations(mutations - 1, mutationFunction(graph), mutationFunction)

  def randomMutationHC(graph: Array[Array[Int]]) = {
    val newGraph = copy(graph)
    val random = scala.util.Random
    val edges = for (
      i <- graph.indices;
      j <- graph.indices;
      if graph(i)(j) == 1 && i < j && i != j && i != j+1 && i+1 != j && !(i == 0 && j == graph.size - 1) && !(j == 0 && i == graph.size - 1)
    ) yield (i, j)
    val complement = for (
      i <- 0 until graph.size;
      j <- 0 until graph.size; 
      if !edges.contains((i, j)) && !edges.contains((j, i)) && i != j && i < j
    ) yield (i, j)

    // 0 -> replace, 1 -> delete, 2 -> add
    // if only hamilton cycle is present, add new edge
    val moveType = {
      if      (edges.isEmpty)      2
      else if (complement.isEmpty) 1
      else                         random.nextInt(3)
    }

    if (moveType == 0) {
      val deleteEdge = edges(random.nextInt(edges.size))
      val newEdge = complement(random.nextInt(complement.size))

      newGraph(deleteEdge._1)(deleteEdge._2) = 0
      newGraph(deleteEdge._2)(deleteEdge._1) = 0
      newGraph(newEdge._1)(newEdge._2) = 1
      newGraph(newEdge._2)(newEdge._1) = 1
    }
    if (moveType == 1) {
      val deleteEdge = edges(random.nextInt(edges.size))
      newGraph(deleteEdge._1)(deleteEdge._2) = 0
      newGraph(deleteEdge._2)(deleteEdge._1) = 0
    }
    if (moveType == 2) {
      val newEdge = complement(random.nextInt(complement.size))

      newGraph(newEdge._1)(newEdge._2) = 1
      newGraph(newEdge._2)(newEdge._1) = 1
    }

    newGraph
  }

  def randomMutation(graph: Array[Array[Int]]) = {
    val newGraph = copy(graph)
    val random = scala.util.Random
    val edges = for (
      i <- graph.indices;
      j <- graph.indices;
      if graph(i)(j) == 1 && i < j 
    ) yield (i, j)
    val complement = for (
      i <- 0 until graph.size;
      j <- 0 until graph.size; 
      if !edges.contains((i, j)) && !edges.contains((j, i)) && i != j && i < j
    ) yield (i, j)

    // 0 -> replace, 1 -> delete, 2 -> add
    val moveType = {
      if      (edges.isEmpty)      2
      else if (complement.isEmpty) 1
      else                         random.nextInt(3)
    }

    if (moveType == 0) {
      val deleteEdge = edges(random.nextInt(edges.size))
      val newEdge    = complement(random.nextInt(complement.size))

      newGraph(deleteEdge._1)(deleteEdge._2) = 0
      newGraph(deleteEdge._2)(deleteEdge._1) = 0
      newGraph(newEdge._1)(newEdge._2) = 1
      newGraph(newEdge._2)(newEdge._1) = 1
    }
    if (moveType == 1) {
      val deleteEdge = edges(random.nextInt(edges.size))

      newGraph(deleteEdge._1)(deleteEdge._2) = 0
      newGraph(deleteEdge._2)(deleteEdge._1) = 0
    }
    if (moveType == 2) {
      val newEdge    = complement(random.nextInt(complement.size))

      newGraph(newEdge._1)(newEdge._2) = 1
      newGraph(newEdge._2)(newEdge._1) = 1
    }

    newGraph
  }
}
