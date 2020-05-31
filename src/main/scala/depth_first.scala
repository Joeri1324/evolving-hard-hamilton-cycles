package experiment

import System.nanoTime
import scala.collection._

trait Solver {

    def name: String

    def solve(
        graph: Array[Array[Int]],
        cutoff: (Int, Long) => Boolean
    ): (Option[Boolean], Int, Long, Option[List[Int]])
}

trait DepthFirst extends Solver {

  /* The branching heurstic. */
  def heuristic: (List[Int], Map[Int, Set[Int]], Set[Int]) => Option[Int]

  /* List of pruning functions that are applied during the pruning phase.
   * Pruning functions should take a graph represented as a map as input
   * and return the deleted edges as a listbuffer. */
  def pruneFuncs: List[
    (Map[Int, mutable.Set[Int]], List[Int]) => mutable.ListBuffer[(Int, Int)]]


  /* Applied the pruning function of pruneFuncs exhaustively untill no more
   * edges can be deleted. */
  def prune(
    edges: Map[Int, mutable.Set[Int]], 
    sol:   List[Int]
  ): mutable.ListBuffer[(Int, Int)] = {

    var changed = true
    val deleted = mutable.ListBuffer[(Int, Int)]()
  
    while (changed) {
      val newDeleted = pruneFuncs.map(f => f(edges, sol)).reduceOption(_ ++ _)
      newDeleted match {
        case Some(mutable.ListBuffer()) => changed = false
        case None                       => changed = false
        case Some(d)                    => deleted ++= d
        
      }
    }
    deleted
  }

  /* Solution pruning removes the edges that cannot be in any Hamilton Cycle
   * because they are already part of the solution. */
  def solutionPruning(
    edges: Map[Int, mutable.Set[Int]], 
    sol: List[Int]
  ): mutable.ListBuffer[(Int, Int)] = {

    if (sol.size > 2) {
      val deleted = mutable.ListBuffer[(Int, Int)]()
      edges(sol.tail.head)
        .filterNot(e => e == sol.head || e == sol.tail.tail.head)
        .foreach(e => {
          edges(sol.tail.head) -= e
          edges(e) -= sol.tail.head
          deleted += ((e, sol.tail.head))
        })
      deleted
    }
    else mutable.ListBuffer()
  }

  /* Neighbour pruning removes all the edges that cannot be in a Hamilton Cycle
   * because they are connected to a vertex this is already connected by two
   * vertices that have a degree of 2. */
  def neighbourPruning(edges: Map[Int, mutable.Set[Int]], sol: List[Int]) = {
    val doubleEdges = { 
      for ( i <- edges.keys; if (edges(i).size == 2)) yield (i) }.toSet

    val del: Map[Int, Set[Int]] = edges.mapValues(n => {
      val doubles = n.filter(a => doubleEdges.contains(a))
      if (doubles.size > 1) n -- doubles.take(2)
      else                  Set[Int]()
    }).filterNot(_._2.isEmpty)

    val result = mutable.ListBuffer[(Int, Int)]()
    
    for {
      (v1, n) <- del.toVector
      v2      <- n
    } {
      edges(v1) -= v2
      edges(v2) -= v1
      result += ((v1, v2))
    }

    result
  }

  /* Path pruning removes all the edges that cannot be in any Hamilton Cycle
   * because they close a cycle of required edges and that cycle is not
   * Hamilton Cycle. */
  def pathPruning(edges: Map[Int, mutable.Set[Int]], sol: List[Int]): 
    mutable.ListBuffer[(Int, Int)] = {

    var changed = false
    
    val doubleEdges1 = for ( i <- edges.keys; if (edges(i).size == 2)) yield (i)
    var doubleEdges = doubleEdges1 toSet
    var deleted = mutable.ListBuffer[(Int, Int)]()

    while (!doubleEdges.isEmpty) {
      var edge1 = edges(doubleEdges.head).head
      var edge2 = edges(doubleEdges.head).tail.head
      var path  = mutable.Set(doubleEdges.head)
       doubleEdges -= doubleEdges.head

      while ( doubleEdges.contains(edge1) ||  doubleEdges.contains(edge2)) {
        if ( doubleEdges.contains(edge1)) {
          path += edge1 
          val temp = edges(edge1).filterNot(e => path.contains(e))
          if (temp.size > 0) edge1 = temp.head else return deleted
        }
        if ( doubleEdges.contains(edge2)) {
          path += edge2 
          val temp = edges(edge2).filterNot(e => path.contains(e))
          if (temp.size > 0) edge2 = temp.head else return deleted
        }
      }
      path.foreach(p =>  doubleEdges -= p)
      if (edges(edge1).contains(edge2)) changed = true;
      if (edges(edge1).contains(edge2)) deleted += ((edge1, edge2))
      edges(edge1) -= edge2
      edges(edge2) -= edge1
      doubleEdges -= edge1
      doubleEdges -= edge2
    }

    deleted
  }

  /* List of functions that are applied to check if a graph is non-hamiltonian
   * a graph is non-hamiltonian if one of the function returns false. */
  def checkFuncs: List[(Map[Int, mutable.Set[Int]]) => Boolean]

  /* Applies all checkFuncs to the graph and checks if they are all true. */
  def check(edges: Map[Int, mutable.Set[Int]]) = 
    checkFuncs.forall(f => f(edges))

  /* Checks if a graph contains a vertex that has a degree lower than 2 */
  def checkOneDegree(edges: Map[Int, mutable.Set[Int]]): Boolean =
    edges.values.forall(_.size >= 2)

  /* checks if a graph is disconnected. */
  def checkDisconnected(edges: Map[Int, mutable.Set[Int]]) = {
    def recurse(
      connectedAll:     Set[Int] = Set(1), 
      connectedToCheck: Set[Int] = Set(1)
    ): Boolean =
      if (connectedAll.size == edges.size) true
      else {
        val n = connectedToCheck.flatMap(e => edges(e)) -- connectedAll

        if (n.isEmpty) false
        else           recurse(connectedAll ++ n, n)
      }
    
    recurse()
  }

  /* Implementation of tarjan's algorithms. The graph is checked for 
   * articulation points. If a graph has an articulation point it is 
   * 1-connected and it isn't if it doesn't have an articulation point */
  def checkOneConnected(edges: Map[Int, mutable.Set[Int]]) = {
    var time = 1

    def dfs(
      points: mutable.ListBuffer[Int],
      visited: mutable.Set[Int] = mutable.Set[Int](),
      vertex: Int = 0, 
      visitedTime: mutable.Map[Int, Int] = mutable.Map[Int, Int](),
      parent: mutable.Map[Int, Int] = mutable.Map[Int, Int](),
      lowTime: mutable.Map[Int, Int] = mutable.Map[Int, Int](),
    ): Unit = {

      var childCount = 0
      var isArticulationPoint = false
      visited += vertex
      visitedTime(vertex) = time
      lowTime(vertex) = time
      time = time + 1

      edges(vertex).foreach(adj => {
        if (parent.contains(adj) && parent(adj) == vertex) {/* do nothing */ }

        else if (visited.contains(adj)) {
          lowTime(vertex) = scala.math.min(lowTime(vertex), visitedTime(adj))
        }
        else {
          childCount = childCount + 1
          parent += ((adj, vertex))
          dfs(points, visited, adj, visitedTime, parent, lowTime)
          if (visitedTime(vertex) <= lowTime(adj)) {
            isArticulationPoint = true
          }
          else {
            lowTime(vertex) = scala.math.min(lowTime(vertex), lowTime(adj))
          }

        }
      })
      if((!parent.contains(vertex) && childCount >= 2) || 
         (parent.contains(vertex) && isArticulationPoint )) {
        points += (vertex);
      }
    }
    val points = mutable.ListBuffer[Int]()
    dfs(points)
    points.isEmpty
  }

 /** Transforms graph into a Map for faster lookup
   *
   * Returns a Map that for a vertex: Int gets a list of all neighbour
   * vertices.
   */
  def createEdgeMap(graph: Array[Array[Int]]): Map[Int, mutable.Set[Int]] = {
    val edges = { for (i <- graph.indices) yield (i, mutable.Set[Int]()) } toMap

    for {
      i <- graph.indices
      j <- graph.indices
      if graph(i)(j) == 1
    } { 
     edges(i) += j
     edges(j) += i 
    }
    edges
  }

 /** Recursive depth-first search solution Hamilton-Cycle  
   * 
   * Checks on each iteration if a child node can be found, if it can
   * that branch is further explored recursively. If there is no solution
   * in a branch it will return false on the left hand side of the or 
   * operator. The right hand side of the operator is a recursive call
   * where this time it doesn't check for that child.
   */
  def solve(
    graph:  Array[Array[Int]],
    cutoff: (Int, Long) => Boolean
  ): (Option[Boolean], Int, Long, Option[List[Int]]) = {

    val edges        = createEdgeMap(graph)
    var iterations   = 0
    val startTime    = nanoTime
    var solution: Option[List[Int]] = None

    def putBack(
      deleted: mutable.ListBuffer[(Int, Int)], 
      edges: Map[Int, mutable.Set[Int]]
    ) = deleted.foreach({ case (v1, v2) => edges(v1) += v2; edges(v2) += v1 })

    def isHamiltonian(sol: List[Int]) = 
      (sol.size == graph.size && graph(sol.head)(sol.last) == 1)

    def recurseSolve(sol: List[Int], checked: Set[Int] = Set()):
      ( Option[Boolean] ) = {

      iterations = iterations + 1
      val deleted = prune(edges, sol)

      if      (cutoff(iterations, startTime)) None
      else if (isHamiltonian(sol))
        { solution = Some(sol); Some(true) }
      else if (!edges.contains(sol.head))
        { putBack(deleted, edges); Some(false) }
      else if (sol.size == graph.size)        {
        recurseSolve(sol.tail, checked + sol.head)
      }
      else {
        heuristic(sol, edges, checked) match {
          case None => { putBack(deleted, edges); Some(false) }
          case Some(i) if !check(edges) => {
            putBack(deleted, edges); Some(false)
          }
          case Some(i)                 => {
            recurseSolve(i :: sol) match {
              case None        => { putBack(deleted, edges); None }
              case Some(true)  => Some(true)
              case Some(false) => 
                { putBack(deleted, edges); recurseSolve(sol, checked + i) }
            }
          }
        }
      }
    }

    if (check(edges)) {
      val hamiltonian = recurseSolve(List(heuristic(Nil, edges, Set()).get))
      (hamiltonian, iterations, nanoTime - startTime, solution)
    } 
    else              (Some(false), 1, nanoTime - startTime, None)
  }
}

object Hueristics {
    /* next vertex gives an option on an integer for the next vertex.
   * The next vertex is selected bases on the first element in the solution
   * a vertex is selected that is not in the solution and is not tried yet.
   * if a heuristic is used the highest or lowest is vertex is selected. */
  def nextVertex(heuristic: String)(
    sol:     List[Int], 
    edges:   Map[Int, Set[Int]], 
    checked: Set[Int]
  ): Option[Int] = {

    val degreeMap = edges.mapValues(_.size)
    val options   = if (sol.isEmpty) edges.keys.toList else edges(sol.head)
      .filter(e => !sol.contains(e) && !checked.contains(e))
      .toList
    val sorted    = options.sortWith((x, y) => {
      if      (heuristic == "low")  degreeMap(x) < degreeMap(y)
      else if (heuristic == "high") degreeMap(x) > degreeMap(y)
      else                          true
    })

    if (sorted.isEmpty) None
    else                Some(sorted.head)
  }

  def nextVertexRemainingUnselected(heuristic: String)(
    sol:     List[Int], 
    edges:   Map[Int, Set[Int]], 
    checked: Set[Int]
  ): Option[Int] = {

    val unseselected = (n: Int) => !checked.contains(n)

    val degreeMap = edges.mapValues(neighbours => neighbours.filter(unseselected).size)
    val options   = if (sol.isEmpty) edges.keys.toList else edges(sol.head)
      .filter(e => !sol.contains(e) && !checked.contains(e))
      .toList
    val sorted    = options.sortWith((x, y) => {
      if      (heuristic == "low")  degreeMap(x) < degreeMap(y)
      else if (heuristic == "high") degreeMap(x) > degreeMap(y)
      else                          true
    })

    if (sorted.isEmpty) None
    else                Some(sorted.head)
  }
}

object ArbitraryHeuristic extends DepthFirst {

  def heuristic  = Hueristics.nextVertex("none")
  def name       = "arbitraryheuristic"
  def pruneFuncs = List()
  def checkFuncs = List()
}

object LowHeuristic extends DepthFirst {

  def heuristic  = Hueristics.nextVertex("low")
  def name       = "lowheuristic"
  def pruneFuncs = List()
  def checkFuncs = List()
}

object HighHeuristic extends DepthFirst {

  def heuristic  = Hueristics.nextVertex("high")
  def name       = "highheuristic"
  def pruneFuncs = List()
  def checkFuncs = List()
}

object PathPruning extends DepthFirst {
  def heuristic  = Hueristics.nextVertex("none")
  def name = "pathpruning"
  def pruneFuncs = List(pathPruning, solutionPruning)
  def checkFuncs = List()
}

object NeighbourPruning extends DepthFirst {
  def heuristic = Hueristics.nextVertex("none")
  def name = "neighbourpruning"
  def pruneFuncs = List(neighbourPruning, solutionPruning)
  def checkFuncs = List()
} 

object NeighbourAndPathPruning extends DepthFirst {
  def heuristic  = Hueristics.nextVertex("none")
  def name = "neighbourandpathpruning"
  def pruneFuncs = List(pathPruning, neighbourPruning, solutionPruning)
  def checkFuncs = List()
}

object CheckOneConnected extends DepthFirst {
  def heuristic  = Hueristics.nextVertex("none")
  def name       = "checkoneconnected"
  def pruneFuncs = List()
  def checkFuncs = List(checkOneConnected)
}

object CheckDisconnected extends DepthFirst {
  def heuristic  = Hueristics.nextVertex("none")
  def name       = "checkdisconnected"
  def pruneFuncs = List()
  def checkFuncs = List(checkDisconnected)
}

object CheckOneDegree extends DepthFirst {
  def heuristic  = Hueristics.nextVertex("none")
  def name       = "checkonedegree"
  def pruneFuncs = List()
  def checkFuncs = List(checkOneDegree)
}

object CheckAll extends DepthFirst {
  def heuristic  = Hueristics.nextVertex("none")
  def name       = "checkall"
  def pruneFuncs = List()
  def checkFuncs = List(checkOneDegree, checkDisconnected, checkOneConnected)
}

object CheckOneConnectedWithPruning extends DepthFirst {
  def heuristic  = Hueristics.nextVertex("none")
  def name       = "checkoneconnectedwithpruning"
  def pruneFuncs = List(pathPruning, neighbourPruning, solutionPruning)
  def checkFuncs = List(checkOneConnected)
}

object CheckDisconnectedWithPruning extends DepthFirst {
  def heuristic  = Hueristics.nextVertex("none")
  def name       = "checkdisconnectedwithpruning"
  def pruneFuncs = List(pathPruning, neighbourPruning, solutionPruning)
  def checkFuncs = List(checkDisconnected)
}

object CheckOneDegreeWithPruning extends DepthFirst {
  def heuristic  = Hueristics.nextVertex("none")
  def name       = "checkonedegreewithpruning"
  def pruneFuncs = List(pathPruning, neighbourPruning, solutionPruning)
  def checkFuncs = List(checkOneDegree)
}

object CheckAllWithPruning extends DepthFirst {
  def heuristic  = Hueristics.nextVertex("none")
  def name       = "checkallwithpruning"
  def pruneFuncs = List(pathPruning, neighbourPruning, solutionPruning)
  def checkFuncs = List(checkOneDegree, checkDisconnected, checkOneConnected)
}

object CheckAllWithPruningLow extends DepthFirst {
  def heuristic  = Hueristics.nextVertex("low")
  def name       = "checkallwithpruninglow"
  def pruneFuncs = List(pathPruning, neighbourPruning, solutionPruning)
  def checkFuncs = List(checkOneDegree, checkDisconnected, checkOneConnected)
}

object CheckAllWithPruningHigh extends DepthFirst {
  def heuristic  = Hueristics.nextVertex("high")
  def name       = "checkallwithpruninglow"
  def pruneFuncs = List(pathPruning, neighbourPruning, solutionPruning)
  def checkFuncs = List(checkOneDegree, checkDisconnected, checkOneConnected)
}