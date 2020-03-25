package experiment
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import ExecutionContext.Implicits.global

object Experiment extends App {
  val numberOfGraphs = 10
  val graphSizes = List((12, 5000), (14, 2000), (16, 1000), (18, 750), (20, 500))


  val algos = List[((Int, Int, Int, Int) => (Int), String)](
    (HillclimbHC.hillclimb, "hillclimb-hc"), (Hillclimb.hillclimb, "hillclimb"), (PPA.ppa, "ppa"), (PPAHC.ppa, "ppa-hc"))

  for ((graphSize, maxEvaluations) <- graphSizes) {
    println(s"Handeling graph size: $graphSize")
    val numberOfEdges = ((scala.math.log(graphSize) + scala.math.log(scala.math.log(graphSize))) * graphSize / 2).toInt

    val funcs = for (
      (algo, name)  <- algos;
      i             <- 1 to numberOfGraphs
    ) yield Future {
      val fitness = algo(i, graphSize, numberOfEdges, maxEvaluations)
      println(s"Graph: $i generated $name fitness: $fitness")
    }

    val all = Future.sequence(funcs)
    Await.result(all, Duration.Inf)
    println("done")

  }
}