package experiment
import java.io.File
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import ExecutionContext.Implicits.global
import java.util.UUID.randomUUID

object Experiment extends App {
  // val hamilton = true
  // val numberOfGraphs = 10
  // //val graphSize = 12
  // //val maxEvaluations = 500
  // // val graphSizes = List((12, 500), (14, 500), (16, 500), (18, 500), (20, 500))
  // val graphSizes = List((12, 3000))
  // val folderName = "super-run" // randomUUID
  // // val (algo, name) = (HillclimbHC.hillclimb _, "hillclimb-hc")
  // //val (algo, name) = (PPAHC.ppa _, "ppa-hc")
  // // val (algo, name) = (Hillclimb.hillclimb _, "hillclimb")
  // val (algo, name) = (PPA.ppa _, "ppa")
  // val graphSize = 12
  // val maxEvaluations = 500
  // val funcs = for (i <- 1 to numberOfGraphs) yield Future {
  //   val fitness = algo(i, graphSize, 5, maxEvaluations, folderName.toString)
  //   println(s"Graph: $i generated $name fitness: $fitness")
  // }

  // val all = Future.sequence(funcs)
  // Await.result(all, Duration.Inf)
  // println("done")

  // val algos = 
  //   if (hamilton)
  //    {
  //     List[((Int, Int, Int, Int, String) => (Int), String)](
  //     (PPAHC.ppa, "ppa-hc"), 
  //     (HillclimbHC.hillclimb, "hillclimb-hc")
  //     )
  //   } else {
  //     List[((Int, Int, Int, Int, String) => (Int), String)](
  //     (Hillclimb.hillclimb, "hillclimb"), (PPA.ppa, "ppa"))
  //   }

  // for ((graphSize, maxEvaluations) <- graphSizes) {
  //   println(s"Handeling graph size: $graphSize")
  //   val numberOfEdges = ((scala.math.log(graphSize) + scala.math.log(scala.math.log(graphSize))) * graphSize / 2).toInt

  //   val funcs = for (
  //     (algo, name) <- algos;
  //     i            <- 1 to numberOfGraphs
  //   ) yield Future {
  //     val fitness = algo(i, graphSize, numberOfEdges, maxEvaluations, folderName.toString)
  //     println(s"Graph: $i generated $name fitness: $fitness")
  //   }

  //   val all = Future.sequence(funcs)
  //   Await.result(all, Duration.Inf)
  //   println("done")

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

  def hamiltonExperiment(graphSizes: List[(Int, Int)], numberOfGraphs: Int, folderName: String, populationSize: Int) = {
    for ((graphSize, maxEvaluations) <- graphSizes) {
      val currentEvalHill = getCurrentEval(folderName, "hillclimb-hc", graphSize)
      val f1 = for (
        i                    <- 1 to numberOfGraphs;
        (graph, totalEvaluations) <- Some(Utils.loadGraph(true, i, graphSize, maxEvaluations, folderName, "hillclimb-hc", currentEvalHill))
      ) yield Future {
        val fitness = HillclimbHC.hillclimb(i, graphSize, maxEvaluations, folderName.toString, graph, totalEvaluations)
        println(s"Graph: $i generated hillclimb-hc fitness: $fitness")
      }

      val currentEvalPpa = getCurrentEval(folderName, "ppa-hc", graphSize)
      val f2 = for (
        i <- 1 to numberOfGraphs;
        (pop, totalEvaluations) <- Some(Utils.loadPopulation(true, i, graphSize, "ppa-hc", maxEvaluations, folderName, populationSize, currentEvalPpa))
      ) yield Future {
        val fitness = PPAHC.ppa(i, graphSize, maxEvaluations, folderName.toString, pop, totalEvaluations)
        println(s"Graph: $i generated ppa-hc fitness: $fitness")
      }

      val all = Future.sequence(f1 ++ f2)
      Await.result(all, Duration.Inf)
      println("done")
    }
  }



  def nonExperiment(graphSizes: List[(Int, Int)], numberOfGraphs: Int, folderName: String, populationSize: Int) = {
    
    for ((graphSize, maxEvaluations) <- graphSizes) {
      val currentEvalHill = getCurrentEval(folderName, "hillclimb", graphSize)
      val f1 = for (
        i                    <- 1 to numberOfGraphs;
        (graph, totalEvaluations) <- Some(Utils.loadGraph(true, i, graphSize, maxEvaluations, folderName, "hillclimb", currentEvalHill))
      ) yield Future {
        val fitness = Hillclimb.hillclimb(i, graphSize, maxEvaluations, folderName.toString, graph, totalEvaluations)
        println(s"Graph: $i generated hillclimb fitness: $fitness")
      }

      val currentEvalPpa = getCurrentEval(folderName, "ppa", graphSize)
      val f2 = for (
        i <- 1 to numberOfGraphs;
        (pop, totalEvaluations) <- Some(Utils.loadPopulation(true, i, graphSize, "ppa", maxEvaluations, folderName, populationSize, currentEvalPpa))
      ) yield Future {
        val fitness = PPA.ppa(i, graphSize, maxEvaluations, folderName.toString, pop, totalEvaluations)
        println(s"Graph: $i generated ppa fitness: $fitness")
      }

      val all = Future.sequence(f1 ++ f2)
      Await.result(all, Duration.Inf)
      println("done")
    }
  }


  val folderName = "super-run" // randomUUID
  val numberOfGraphs = 10
  val populationSize = 10
 // val graphSizes = List((12, 100), (14, 100)) //, (16, 500), (18, 500), (20, 500))
  // hamiltonExperiment(graphSizes, numberOfGraphs, folderName, populationSize)
  val graphSizes = List((14, 5000), (16, 1000)) // , (18, 300), (20, 300))
  hamiltonExperiment(graphSizes, numberOfGraphs, folderName, populationSize)
}