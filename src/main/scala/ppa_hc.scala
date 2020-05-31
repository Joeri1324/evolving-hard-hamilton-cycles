package experiment

import System._


object PPAHC {

  val random = scala.util.Random

  def cutoff(maxTime: Long, maxIter: Int, timeOrIterations: String = "time")
      (curIter: Int, startTime: Long): Boolean =
      if (timeOrIterations == "time") nanoTime - startTime > maxTime
      else                            curIter > maxIter

    def ppa(
        index: Int,
        graphSize: Int,
        maxEvaluations: Int,
        folderName: String,
        l: Seq[Array[Array[Int]]],
        newEvaluations: Int,
        algorithm: Solver

    ): Int = {
        val k = 20
        val numberOfGens = maxEvaluations / 25
        val populationSize = 10
        var p = scala.collection.mutable.ArraySeq(l:_*)
        var bestGraph = p.head
        var maxFitness = 0
        var maxHamiltonian = true
        var bestPath: List[Int] = Nil
        var chicke = 0
        var n = p.map(graph => algorithm.solve(graph, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations")))

        for (i <- 0 until numberOfGens) {

            p = scala.collection.mutable.ArraySeq(p.indices.sortWith((a, b) => n(a)._2 > n(b)._2).map(i => p(i)): _*)

            var n_sorted = n.sortWith((a, b) =>  a._2 > b._2)

            // number one, do 6 times, 1 mutation
            for (_ <- 0 until 6) {
              val r = Utils.randomMutationHC(p(0))
              val (hamiltonian, recursions, time, path) = algorithm.solve(r, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
              if (recursions >= n(0)._2) {
                n(0) = (hamiltonian, recursions, time, path)
                p(0) = r
              }
              chicke = chicke + 1
            }
            // number two, do 5 times, 2 mutations
            for (_ <- 0 until 5) {
              val r = Utils.randomMutations(2, p(1), Utils.randomMutationHC)
              val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(r, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
              if (recursions >= n(1)._2) {
                  n(1) = (hamiltonian, recursions, time, path)
                  p(1) = r
              }
              chicke = chicke + 1
            }
            // number three, do 4 times, 5 mutations
            for (_ <- 0 until 4) {
              val r = Utils.randomMutations(5, p(2), Utils.randomMutationHC)
              val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(r, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
              if (recursions >= n(2)._2) {
                  n(2) = (hamiltonian, recursions, time, path)
                  p(2) = r
              }
              chicke = chicke + 1
            }
            // number four, do 3 times, 5 mutations
            for (_ <- 0 until 3) {
              val r = Utils.randomMutations(k, p(3), Utils.randomMutationHC)
              val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(r, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
              if (recursions >= n(3)._2) {
                  n(3) = (hamiltonian, recursions, time, path)
                  p(3) = r
              }
              chicke = chicke + 1
            }
            // number five, do 2 times, 10 mutations
            for (_ <- 0 until 2) {
              val r = Utils.randomMutations(10, p(4), Utils.randomMutationHC)
              val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(r, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
              if (recursions >= n(4)._2) {
                  n(4) = (hamiltonian, recursions, time, path)
                  p(4) = r
              }
              chicke = chicke + 1
            }

            // bottom 8
            for (i <- (5 until n.size)) {
                val r = Utils.randomMutations(20, p(i.toInt), Utils.randomMutationHC)
                val (hamiltonian, recursions, time, path) = CheckAllWithPruningLow.solve(r, cutoff(10000.toLong * 10000.toLong, 1000000000, "iterations"))
                if (recursions >= n_sorted(i.toInt)._2 && chicke < maxEvaluations) {
                    p(i.toInt) = r
                    n(i) = (hamiltonian, recursions, time, path)
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
            for (j <- p.indices) {
                val hams = n_sorted.head._1 match {
                    case Some(true) => { true }
                    case Some(false) => { false }
                    case None => { false}
                }
                var sols = n_sorted.head._4 match {
                    case Some(solution) => {  solution }
                    case None => { List() }
                }
                val json = GraphGenerator.graphToJson(j, p(j), n(j)._2, hams, sols)
                GraphGenerator.writeGraphToFile(s"results/$folderName/ppa-hc/$graphSize/$newEvaluations/$index/$j", i, json)
            }
        }
        return maxFitness
    }

    val populationSize = 10
}