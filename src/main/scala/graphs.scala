package experiment

import spray.json._
import DefaultJsonProtocol._
import java.io.File
import java.io.PrintWriter
import java.nio.file.{Path, Paths, Files}
import scala.util.Try

/** JSON representation of a graph
  */
case class Vertex(id: Int)
case class Endpoint(id: Int)
case class Edges(endpoints: List[Endpoint])
case class Graph(
  identifier:      Int, 
  vertices:        List[Vertex], 
  edges:           List[Edges], 
  connectivityMap: Map[String, Int],
  size:            Int,
  recursions:      Int,
  hamiltonian:     Boolean,
  var path:        List[Int] = Nil,
) {

  def array = {
    val graph = 
      { for {_ <- 0 until size} yield ({ for {_ <- 0 until size} yield (0) } toArray) } toArray

    edges.foreach(e => {
      val v1 = e.endpoints(0).id
      val v2 = e.endpoints(1).id
      graph(v1)(v2) = 1; graph(v2)(v1) = 1
    })
    graph
  }

  // def randomMutation = {
  //   val random = scala.util.Random

  //   val edgeset = { for (e <- edges) yield (e.endpoints(0).id, e.endpoints(1).id) } toSet

  //   val complement = for (
  //     i <- 0 until size;
  //     j <- 0 until size; 
  //     if !edgeset.contains((i, j)) && !edgeset.contains((j, i)) && i != j && i < j
  //   ) yield (i, j)

  //   println(edges.size * complement.size)

  //   val deleteEdge = random.nextInt(edges.size)
  //   val newEdge    = random.nextInt(complement.size)

  //   val n = Edges(List(Endpoint(complement(newEdge)._1), Endpoint(complement(newEdge)._2)))
  //   val newEdges = edges.slice(0, deleteEdge) ::: edges.slice(deleteEdge + 1, edges.size)

  //   (
  //     Graph(identifier, vertices, n :: newEdges, connectivityMap, size),
  //     edges(deleteEdge),
  //     complement(newEdge)
  //   )
  // }


  def relativeEdgeAmount = {
    edges.size.toFloat / size * 2
  }
}
case class GraphList(graphs: List[Graph])

object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val endpointFormat = jsonFormat(Endpoint, "id")
  implicit val edgesFormat    = jsonFormat(Edges, "endpoints")
  implicit val vertexFormat   = jsonFormat(Vertex, "id")
  implicit val graphFormat    = jsonFormat(Graph, "identifier", "vertices", "edges",
    "connectivityMap", "size", "recursions", "hamiltonian", "path")
  implicit object graphListJsonFormat extends RootJsonFormat[GraphList] {
    def read(value: JsValue) = GraphList(value.convertTo[List[Graph]])
    def write(value: GraphList) = JsArray(
      value.graphs.map(graph => JsObject(
        "hamiltonian" -> JsNumber({ if (graph.hamiltonian) 1 else 0 }),
        "identifier" -> JsNumber(graph.identifier),
        "recursions" -> JsNumber(graph.recursions),
        "vertices"   -> JsArray(
          graph.vertices.map(v => JsObject("id" -> JsNumber(v.id)))
        ),
        "edges" -> JsArray (
          graph.edges.map(e => JsObject("endpoints" -> JsArray(
            JsObject("id" -> JsNumber(e.endpoints(0).id)),
            JsObject("id" -> JsNumber(e.endpoints(1).id)),
          ))
          )
        ),
        "connectivityMap" -> JsObject(graph.connectivityMap.map(c => {
          c._1 -> JsNumber(c._2)
        })),
        "path" -> JsArray(graph.path.map(x => JsNumber(x)))
      ))
    )
  } 
}

import MyJsonProtocol._
import DefaultJsonProtocol._

object GraphReader {

//   def graphsFromFile(fileName: String) = 
//     scala.io.Source.fromFile(fileName)
//       .mkString
//       .parseJson
//       .convertTo[GraphList]
//       .graphs

  def handleFile(name: String) = 
    scala.io.Source.fromFile(name)
      .mkString
      .parseJson
      .convertTo[Graph]

  // def graphsFromFolder(folder: String) = {
  //   def handleFile(name: String) = 
  //     scala.io.Source.fromFile(name)
  //       .mkString
  //       .parseJson
  //       .convertTo[Graph]
  // }

  //   for {
  //     filename <- new File(folder).listFiles
  //     graph    <- Some(handleFile(filename.toString))
  //   } yield graph
  // }

}

object GraphGenerator {

  /** Generates a graph represented as an Array[Array[Int]] of size [size]
    *
    * An Array[Array[Int]] of size [size x size] is initialized with only zeros,
    * then recursively an edge is added to the Array untill [amountOfEdges] is reached
    * and the Array[Array[Int]] will be returned.
    */
  def genGraph(size: Int, amountOfEdges: Int): Array[Array[Int]] = {

    val r       = scala.util.Random
    val indices = { for (
      i <- 0 until size; 
      j <- 0 until size;
      if(i > j)) yield (i, j) }.toVector
    val graph =
      {for {i <- 0 until size} yield {for {j <- 0 until size} yield 0} toArray} toArray

    def recursiveGenGraph(currentEdges: Int, indices: Vector[(Int, Int)],
      graph: Array[Array[Int]]): Array[Array[Int]] = {

      require(currentEdges <= amountOfEdges, 
        "Shouldnt be instantiated higher than total amount")

      if (currentEdges == amountOfEdges || indices.isEmpty) graph
      else {
        val index   = r.nextInt(indices.size)
        val newEdge = indices(index)
        recursiveGenGraph(
          currentEdges + 1,
          indices.slice(0, index) ++ indices.slice(index + 1, indices.size),
          graph
            .updated(newEdge._1, graph(newEdge._1).updated(newEdge._2, 1))
            .updated(newEdge._2, graph(newEdge._2).updated(newEdge._1, 1)))
      }
    }

    if (amountOfEdges > 0) recursiveGenGraph(0, indices, graph)
    else                   graph
  }

  def genGraphWithHamiltonCycle(size: Int, amountOfEdges: Int): Array[Array[Int]] = {
    val indices = { for (
      i <- 0 until size; 
      j <- 0 until size;
      if(i > j)) yield (i, j) }.toVector
    var graph =
      {for {i <- 0 until size} yield {for {j <- 0 until size} yield 0} toArray} toArray

    for (i <- 0 until (size - 1)) {
      graph = graph
        .updated(i, graph(i).updated(i + 1, 1))
        .updated(i + 1, graph(i + 1).updated(i, 1))
    }
    graph = graph
        .updated(size - 1, graph(size - 1).updated(0, 1))
        .updated(0, graph(0).updated(size - 1, 1))

    val r = scala.util.Random

    def recursiveGenGraph(currentEdges: Int, graph: Array[Array[Int]]): Array[Array[Int]] = {
      val edges = for (
        i <- graph.indices;
        j <- graph.indices;
        if graph(i)(j) == 1 && i < j
      ) yield (i, j)

      val indices = for (
        i <- 0 until graph.size;
        j <- 0 until graph.size; 
        if !edges.contains((i, j)) && !edges.contains((j, i)) && i != j && i < j
      ) yield (i, j)

      if (currentEdges == amountOfEdges || indices.isEmpty) graph
      else {
        val index   = r.nextInt(indices.size)
        val newEdge = indices(index)
        recursiveGenGraph(
          currentEdges + 1,
          graph
            .updated(newEdge._1, graph(newEdge._1).updated(newEdge._2, 1))
            .updated(newEdge._2, graph(newEdge._2).updated(newEdge._1, 1)))
      }
    }
    
    return recursiveGenGraph(12, graph)
  }

  def printGraph(graph: Array[Array[Int]]) = {
    for (row <- graph) {
      println(row.mkString)
    }
  }

  /** Writes filecontent to path and generated filename based on the id.
    * 
    * If the folders in path do not exist yet they are created.
    */
  def writeGraphToFile(path: String, id: Int, fileContent: String): Unit = {
    def createDir(path: String): File = {
      val dir = new File(path)
      dir.mkdirs()
      dir
    }
    createDir(path)
    val pw = new PrintWriter(new File(s"$path/$id.json"))
    try pw.write(fileContent) finally pw.close()
  }

  /** Converts graph to json format as string.
   */
  def graphToJson(identifier: Int, graph: Array[Array[Int]], recursions: Int, hamiltonian: Boolean, path: List[Int]): String = {
    val vertices = { for (i <- graph.indices) yield Vertex(i) } toList
    val edges = { for (
      i <- graph.indices;
      j <- graph.indices;
      if graph(i)(j) == 1 && i < j 
    ) yield(Edges(List(Endpoint(i), Endpoint(j)))) } toList
    val degreeMap = { for (i <- graph.indices) yield (i.toString, graph(i).count(_ == 1)) } toMap

    Graph(identifier, vertices, edges, degreeMap, graph.size, recursions, hamiltonian, path)
      .toJson
      .prettyPrint
  }

  // require(Try(args(0).toInt).isSuccess, "Input graph size should be int")
  // val graphSize      = args(0).toInt
  // val amountOfGraphs = 20
  // val maxEdges       = {0 to (graphSize - 1)}.reduce(_ + _)

  // for {
  //   amountOfEdges <- 1 to maxEdges;
  //   graphNumber   <- 0 to amountOfGraphs;
  //   graphId       <- Some(((amountOfEdges - 1) * amountOfGraphs) + graphNumber)
  // } {
  //   val json = graphToJson(graphId, genGraph(graphSize, amountOfEdges))
  //   writeGraphToFile(s"src/main/resources/indexed-$graphSize-node-test-set", graphId, json)
  // }
}
