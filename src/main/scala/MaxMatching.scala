import java.io.{File, PrintWriter}
import java.nio.file.Paths


import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.io.Source
import scala.math

object MaxMatching extends App{
  val none = 32767
  //println(f)
  val source = Source.fromResource("in.txt")
  val read = source
    .getLines
    .map(s => s.split(" ").map(t => t.toInt).toVector)
    .toVector
  source.close()
  val (xsize, ysize) = (read(0)(0), read(0)(1))
  val array = read(2)
  //println(s"$xsize $ysize")
  //println(array)
  val firstPart = array.take(xsize)
  val borderList = firstPart.sliding(2).toVector :+ Vector(firstPart.last, array.length)
  //println(borderList)
  val gPairs = borderList
    .map(p => array.slice(p(0)-1, p(1)-1))
    .zipWithIndex
    .map(p => (p._2+1, p._1.map(n => n+xsize)))
  val g1 = gPairs.toMap
  val g2 =  g1 + (0 -> Vector.range(1, xsize+1))
  val t = Vector
    .range(xsize+1, xsize + ysize +1)
    .map(n => (n -> Vector(xsize + ysize+1)))
  val g = g2 ++ t
  var gFlow = g.map(p => (p._1, Map.from(p._2.map(v =>
      (v, 0/*flow*/)
  ))))
  /*var gFlow: Map[Int, Map[Int, Int]] = Map(
    0 -> Map(1 -> 0, 2 -> 1, 3 -> 1),
    5 -> Map(7 -> 0),
    1 -> Map(4 -> 0),
    6 -> Map(7 -> 1),
    2 -> Map(4 -> 1, 5 -> 0),
    3 -> Map(5 -> 1, 6 -> 1),
    4 -> Map(7 -> 1))*/
  /*var gFlow: Map[Int, Map[Int, Int]] = Map(
    0 -> Map(1 -> 0, 2 -> 1, 3 -> 1),
    5 -> Map(7 -> 1),
    1 -> Map(4 -> 0),
    6 -> Map(7 -> 1),
    2 -> Map(4 -> 1, 5 -> 0),
    3 -> Map(5 -> 1, 6 -> 1),
    4 -> Map(7 -> 1))*/
  //println(gFlow)
  val c = 1
  val sMin = Int.MaxValue
  var vertexList = Stack[(Int, Int, Boolean, Int)]((-1, 0/*vertex*/, true, Int.MaxValue))
  //println(dfs(HashSet[Int](), List[Int]()))

  /*true - прямая false - обратная*/
  //println(gFlow.filter(p => p._2.keys.toSet.contains(4)).keys)
  var r = (0,List[Int]())
  //var vertexList = Stack[(Int, Int, Boolean, Int)]((-1, 0/*vertex*/, true, Int.MaxValue))
  do{
    vertexList = Stack[(Int, Int, Boolean, Int)]((-1, 0/*vertex*/, true, Int.MaxValue))
    r = dfs(HashSet[Int](), List[Int]())
    //println(r)
    //println(gFlow)
  }while(r._1 != -3)

  val ress = gFlow
    .filter(p => p._1!=0 && !p._2.keys.toSet.contains(xsize + ysize+1))
    .flatMap(p => p._2.map(t => (p._1, t._1, t._2)))
    .filter(t => t._3==1)
    .map(t =>"|"+t._1 +" "+t._2+"|")
  val letter = if (ress.size==xsize) "Y\n"+ ress.mkString(" ") else "N"

  val resourceDirectory = Paths.get("src", "main", "resources", "out.txt")
  val absolutePath = resourceDirectory.toFile.getAbsolutePath
  val fout = new File(absolutePath)
  //println(fout)
  val pw = new PrintWriter(fout)
  pw.write(letter)
  pw.close()

  def dfs(visited:HashSet[Int], res1:List[Int]): (Int, List[Int]) ={
    if (vertexList.nonEmpty){
      val v1 = vertexList.pop()
      val v = v1._2
      val pred = v1._1
      val res = if (pred != -1 || v!=0) res1:+v else res1
      val hMin = v1._4
      //println(v)
      //println(visited)
      //
      // println(vertexList)
      val f = if (pred!= -1) {if(v1._3) gFlow(pred)(v) else gFlow(v)(pred)} else -4
      if (v==7){
        val h = if (v1._3) c - f else f
        //println("here is 7")
        //println(v1._3)
        //println(h)
        if (h > 0) {
          val resH = h min hMin
          if (pred!= -1){
            gFlow = if(v1._3) gFlow.updated(pred, gFlow(pred).updated(v, gFlow(pred)(v) + resH))
            else gFlow.updated(v, gFlow(v).updated(pred,
              gFlow(v)(pred) - resH))
          }
          (resH, res)
        } else{
          (-10, res)
          //val newVisited = visited
          //vertexList.popAll()
          //vertexList.push((0, true))
          //dfs(newVisited, res)
        }
      }else{
        val newVisited = visited+v
        val h = if (v1._3) c - f else f
        //println(h)
        if (h>0){
          val resH = h min hMin
          val rev = gFlow
            .filter(p => p._2.keys.toSet.contains(v)).keys
            .filter(n => !visited.contains(n))
            .map(t => (v, t, false, resH)).toSeq.reverse
          vertexList.pushAll(rev)
          val straight = gFlow(v).keys
            .filter(n => !visited.contains(n)).map(t => (v, t, true, resH))
            .toSeq.reverse
          vertexList.pushAll(straight)

          val resDfs = dfs(newVisited, res)
          val hp = if (pred != -1) h min resDfs._1 else resDfs._1
          //println("++++++")
          //println(v)
          //println(v1)
          //println(hp)
          if (hp > 0) {
            if (pred!= -1){
              gFlow = if(v1._3) gFlow.updated(pred, gFlow(pred).updated(v, gFlow(pred)(v) + hp))
              else gFlow.updated(v, gFlow(v).updated(pred,
                gFlow(v)(pred) - hp))
            }
            (hp, resDfs._2)
          }
          else {
            val newVisited = visited+v
            //vertexList.popAll()
            //vertexList.push((0, true))
            dfs(newVisited, res)
          }
        }else {
          val newVisited = visited+v
          //vertexList.popAll()
          //vertexList.push((0, true))
          dfs(newVisited, res)
        }

      }
    }else (-3, List[Int]())
  }
}
