package experiment
import java.io.File
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import ExecutionContext.Implicits.global
import java.util.UUID.randomUUID

object Experiment extends App {

  def getCurrentEval(folderName: String, algorithmName: String, graphSize: Int): Option[Int] = {
    val folder = s"results/$folderName/$algorithmName/$graphSize"
    val r = scala.util.Random
    val d = new File(folder)
    if (d.exists) {
      val files = d.listFiles.toList
      return Some(files.map(f => f.toString.split("/").last.toInt).max)
    }
    else {
      return None
    }
  }

  def calcNumberOEdges(graphSize: Int, i: Int, numberOfGraphs: Int, komlosBound: Boolean = false) = {
    val maximumEdgesPossible = graphSize * (graphSize - 1) / 2
    
    if (komlosBound) scala.math.ceil((scala.math.log(graphSize) + scala.math.log(scala.math.log(graphSize))) * graphSize / 2).toInt
    else             (maximumEdgesPossible * (i - 1) / numberOfGraphs).toInt
  }

  def experiment(graphSizes: List[(Int, Int)], numberOfGraphs: Int, folderName: String, populationSize: Int, algorithm: Solver) = {
    var functions = Seq[scala.concurrent.Future[Unit]]()
    for ((graphSize, maxEvaluations) <- graphSizes) {
      val currentEvalHill = getCurrentEval(folderName, "hillclimb-hc", graphSize)
      val f1 = for (
        i                    <- 1 to numberOfGraphs;
        numberOfEdges        <- Some(calcNumberOEdges(graphSize, i, numberOfGraphs));
        (graph, totalEvaluations) <- Some(Utils.loadGraph(numberOfEdges, true, i, graphSize, maxEvaluations, folderName, "hillclimb-hc", currentEvalHill))
      ) yield Future {
        val fitness = HillclimbHC.hillclimb(i, graphSize, maxEvaluations, folderName.toString, graph, totalEvaluations, algorithm)
        println(s"Graph: $i generated hillclimb-hc $graphSize fitness: $fitness")
      }

      val currentEvalPpa = getCurrentEval(folderName, "ppa-hc", graphSize)
      val f2 = for (
        i <- 1 to numberOfGraphs;
        numberOfEdges        <- Some(calcNumberOEdges(graphSize, i, numberOfGraphs));
        (pop, totalEvaluations) <- Some(Utils.loadPopulation(numberOfEdges, true, i, graphSize, "ppa-hc", maxEvaluations, folderName, populationSize, currentEvalPpa))
      ) yield Future {
        val fitness = PPAHC.ppa(i, graphSize, maxEvaluations, folderName.toString, pop, totalEvaluations, algorithm)
        println(s"Graph: $i generated ppa-hc $graphSize fitness: $fitness")
      }

      val currentEvalHill2 = getCurrentEval(folderName, "hillclimb", graphSize)
      val f3 = for (
        i                    <- 1 to numberOfGraphs;
        numberOfEdges        <- Some(calcNumberOEdges(graphSize, i, numberOfGraphs));
        (graph, totalEvaluations) <- Some(Utils.loadGraph(numberOfEdges, false, i, graphSize, maxEvaluations, folderName, "hillclimb", currentEvalHill2))
      ) yield Future {
        val fitness = Hillclimb.hillclimb(i, graphSize, maxEvaluations, folderName.toString, graph, totalEvaluations, algorithm)
        println(s"Graph: $i generated hillclimb $graphSize  fitness: $fitness")
      }

      val currentEvalPpa2 = getCurrentEval(folderName, "ppa", graphSize)
      val f4 = for (
        i <- 1 to numberOfGraphs;
        numberOfEdges        <- Some(calcNumberOEdges(graphSize, i, numberOfGraphs));
        (pop, totalEvaluations) <- Some(Utils.loadPopulation(numberOfEdges, false, i, graphSize, "ppa", maxEvaluations, folderName, populationSize, currentEvalPpa2))
      ) yield Future {
        val fitness = PPA.ppa(i, graphSize, maxEvaluations, folderName.toString, pop, totalEvaluations, algorithm)
        println(s"Graph: $i generated ppa $graphSize fitness: $fitness")
      }
      functions = functions ++ f1 ++ f2 ++ f3 ++ f4
    }
    val all = Future.sequence(functions)
    Await.result(all, Duration.Inf)
    println("done")
  }

  val folderName = "final-run-10-12-14" // randomUUID
  val numberOfGraphs = 20
  val populationSize = 10
  val graphSizes = List((8, 2000), (9, 2000), (10, 2000), (11, 2000), (12, 2000), (13, 2000, (14, 2000)))
  experiment(graphSizes, numberOfGraphs, folderName, populationSize,  CheckAllWithPruningLow)
}
