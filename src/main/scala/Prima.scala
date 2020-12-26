import java.io.File

import scala.collection.immutable.HashSet
import scala.io.Source

import java.io._
import java.nio.file.Paths

object Prima extends App {
  val none = 32767
  //println(f)
  val source = Source.fromResource("in.txt")
  val read = source
    .getLines
    .map(s => s.split(" ").map(t => t.toInt).zipWithIndex.toVector)
    .toVector
  source.close()
  val n = read.head(0)._1
  val g = read.tail
  var ostov = Vector.fill(n)(Vector[Int]())
  //println(ostov.length)
  var visited = HashSet[Int](0)
  var weight = 0
  while (visited.size!=n){
    //println(visited)
    val min =
      visited.map(n => {
        val suitable = g(n).filter(p => p._1!=none && !visited.contains(p._2))
        if (suitable.nonEmpty){
          val m = suitable.minBy(p => p._1)
          (m._1/*weigth*/, m._2/*vertex index*/, n)
        }else{
          (32767, -1, n)
        }
    }).minBy(pair => pair._1)
    //println(min)
    weight += min._1
    visited += min._2
    ostov = ostov.updated(min._3, ostov(min._3):+ min._2)
    ostov = ostov.updated(min._2, ostov(min._2):+ min._3)
  }
  //println(ostov.map(v => v.map(i => i+1)))
  //println(weight)

  val res1 = ostov
    .map(v => v.map(i => i+1) :+ 0)
    .map(ar => ar.mkString(" ")) :+ weight.toString
  val res = res1.mkString("\n")
  //println(res)

  val resourceDirectory = Paths.get("src", "main", "resources", "out.txt")
  val absolutePath = resourceDirectory.toFile.getAbsolutePath
  val fout = new File(absolutePath)
  //println(fout)
  val pw = new PrintWriter(fout)
  pw.write(res)
  pw.close()
}
