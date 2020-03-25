package experiment

import System._


object PPAHC { // {extends App {

  val random = scala.util.Random

  def cutoff(maxTime: Long, maxIter: Int, timeOrIterations: String = "time")
      (curIter: Int, startTime: Long): Boolean =
      if (timeOrIterations == "time") nanoTime - startTime > maxTime
      else                            curIter > maxIter

    def ppa(
        index: Int,
        graphSize: Int,
        amountOfEdges: Int,
        maxEvaluations: Int,
        // k: Int = 20,
        // y: Int = 5,
    ): Int = {
        val k = 20
        val numberOfGens = maxEvaluations / 20
        val populationSize = 10
        val l = for (_ <- 0 until populationSize) yield GraphGenerator.genGraphWithHamiltonCycle(graphSize, amountOfEdges)
        var p = scala.collection.mutable.ArraySeq(l:_*)
        var bestGraph = p.head
        var maxFitness = 0
        var maxHamiltonian = true
        var bestPath: List[Int] = Nil
        var chicke = 0
        Utils.delete(s"results/ppa-hc/$maxEvaluations-evaluations/$graphSize-size/$index")
        var n = p.map(graph => CheckAllWithPruningLow.solve(graph, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations")))

        for (i <- 0 until numberOfGens) {
            var n = p.map(graph => CheckAllWithPruningLow.solve(graph, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations")))

            p = scala.collection.mutable.ArraySeq(p.indices.sortWith((a, b) => n(a)._2 > n(b)._2).map(i => p(i)): _*)

            var n_sorted = n.sortWith((a, b) =>  a._2 > b._2)

            // number one, do 10 times
            for (_ <- 0 until 7) {
              val r = Utils.randomMutationHC(p(0))
              val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(r, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
              if (recursions >= n(0)._2) {
                  p(0) = r
              }
              chicke = chicke + 1
            }
            // number two, do 
            for (_ <- 0 until 5) {
              val r = Utils.randomMutationHC(p(1))
              val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(r, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
              if (recursions >= n(1)._2) {
                  p(1) = r
              }
              chicke = chicke + 1
            }

            // do the top 10 %
            // val bound = scala.math.round(0.2 * n.size)
            // for (i <- 0 to bound.toInt) {
            //     for (_ <- 0 to scala.math.round(y / (i + 1)).toInt) {
            //         val r = Utils.randomMutation(p(i.toInt))
            //         val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(r, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
            //         if (recursions >= n(i.toInt)._2) {
            //             p(i.toInt) = r
            //         }
            //         chicke = chicke + 1
            //     }
            // }

            // bottom 8
            for (i <- (2 until n.size)) {
                val r = Utils.randomMutations(k, p(i.toInt), Utils.randomMutationHC)
                val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(r, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
                if (recursions >= n_sorted(i.toInt)._2 && chicke < maxEvaluations) {
                    p(i.toInt) = r
                }
                chicke = chicke + 1
            }
            // do the bottom 90 %
            bestGraph = p.head
            maxFitness = n.head._2
            n_sorted.head._1 match {
                case Some(true) => { maxHamiltonian = true }
                case Some(false) => { maxHamiltonian = false }
                case None => { maxHamiltonian = false}
            }
            n_sorted.head._4 match {
                case Some(solution) => { bestPath = solution }
                case None => { bestPath = List() }
            }
            val json = GraphGenerator.graphToJson(i, bestGraph, maxFitness, maxHamiltonian, bestPath)
            GraphGenerator.writeGraphToFile(s"results/ppa-hc/$maxEvaluations-evaluations/$graphSize-size/$index", i, json)
        }
        return maxFitness
        // val l = for (_ <- 0 to populationSize) yield GraphGenerator.genGraphWithHamiltonCycle(graphSize, amountOfEdges)
        // var p = scala.collection.mutable.ArraySeq(l:_*)
        // var bestGraph = p.head
        // var maxFitness = 0
        // var maxHamiltonian = true
        // var bestPath: List[Int] = Nil
        // var chicke = 0
        // val numberOfGens = maxEvaluations / 20
        // Utils.delete(s"results/ppa-hc/$maxEvaluations-evaluations/$graphSize-size/$index")
    
        // var n = p.map(graph => CheckAllWithPruningLow.solve(graph, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations")))

        // for (i <- 0 to numberOfGens) {
        //     var n = p.map(graph => CheckAllWithPruningLow.solve(graph, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations")))

        //     p = scala.collection.mutable.ArraySeq(p.indices.sortWith((a, b) => n(a)._2 > n(b)._2).map(i => p(i)): _*)

        //     var n_sorted = n.sortWith((a, b) =>  a._2 > b._2)
        //     // do the top 10 %
        //     val bound = scala.math.round(0.2 * n.size)
        //     for (i <- 0 to bound.toInt) {
        //         for (_ <- 0 to scala.math.round(y / (i + 1)).toInt) {
        //             val r = Utils.randomMutationHC(p(i.toInt))
        //             val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(r, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
        //             if (recursions >= n(i.toInt)._2) {
        //                 p(i.toInt) = r
        //             }
        //             chicke = chicke + 1
        //         }
        //     }
        //     for (i <- (bound + 1) until n.size) {
        //         val r = Utils.randomMutations(k, p(i.toInt), Utils.randomMutationHC)
        //         val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(r, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
        //         if (recursions >= n_sorted(i.toInt)._2 && chicke < maxEvaluations) {
        //             p(i.toInt) = r
        //         }
        //         chicke = chicke + 1
        //     }
        //     // do the bottom 90 %
        //     bestGraph = p.head
        //     maxFitness = n.head._2
        //     n_sorted.head._1 match {
        //         case Some(true) => { maxHamiltonian = true }
        //         case Some(false) => { maxHamiltonian = false }
        //         case None => { maxHamiltonian = false}
        //     }
        //     n_sorted.head._4 match {
        //         case Some(solution) => {
        //           bestPath = solution
        //         }
        //         case None => { bestPath = List() }
        //     }
        //     val json = GraphGenerator.graphToJson(i, bestGraph, maxFitness, maxHamiltonian, bestPath)
        //     GraphGenerator.writeGraphToFile(s"results/ppa-hc/$maxEvaluations-evaluations/$graphSize-size/$index", i, json)
        // }
        // return (bestGraph, maxFitness, maxHamiltonian)
    }
    
    

    val populationSize = 10
    // val numberOfGens = 26
    // val graphSize = 14
    // val numberOfEdges = ((scala.math.log(graphSize) + scala.math.log(scala.math.log(graphSize))) * graphSize / 2).toInt
    // val k: Int = 5
    // val y = 5
    // val experiment_times = 10
    // val maxEvaluations = 5000

    // for (i <- 0 until experiment_times) {
    //     println(s"Starting ppa $i")
    //     val (graph, fitness, hamiltonian) = ppa(
    //         i,
    //         graphSize,
    //         numberOfEdges,
    //         maxEvaluations
    //     )
    //     println(s"genereated: $fitness")
    // }
}