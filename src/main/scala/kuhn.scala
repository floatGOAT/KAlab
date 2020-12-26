import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.collection.mutable

object kuhn extends App {
  @tailrec
  def readToMap(
                 g:Map[Int, Vector[Int]], gReverse:Map[Int, Vector[Int]],  count:Int, border:Int):
  (Map[Int, Vector[Int]], Map[Int, Vector[Int]]) ={
      if (count>border)
        (g, gReverse)
      else {
        val p  = scala.io.StdIn.readLine().split(" ")
        val (a, b) = (p(0).toInt, p(1).toInt)
        (g.get(a), gReverse.get(b)) match {
          case (Some(value), Some(value1)) =>
            readToMap(g + (a -> (value :+ b)), gReverse + (b -> (value1 :+ a)), count+1, border)
          case (Some(value), None) =>
            readToMap(g + (a -> (value :+ b)), gReverse + (b -> Vector(a)), count+1, border)
          case (None, Some(value1)) =>
            readToMap(g + (a -> Vector(b)), gReverse + (b -> (value1 :+ a)), count+1, border)
          case (None, None) =>
            readToMap(g + (a -> Vector(b)),gReverse + (b -> Vector(a)), count+1, border)
        }
      }
  }


  def tryFindChain(v: Int, used:HashSet[Int], mt:Vector[Int]): (Boolean, HashSet[Int], Vector[Int]) = {
    if (used.contains(v)) return (false, used, mt)
    val newUsed = used + v;
    var i = 0
    while (i < g(v).size) {
      val to = g(v)(i)
      if ((mt(to) == -1) || tryFindChain(mt(to), newUsed, mt)._1) {
        val newMt = mt.updated(to, v)
        return (true, newUsed, newMt)
      }
      i += 1
    }
    (false, used, mt)
  }

  @tailrec
  def goCicle(used:HashSet[Int], mt:Vector[Int], v:Int, border:Int): Int ={
    if (v>border) {
      //print(used)
      //print(mt)
      //print(mt.count(t => t != -1))
      mt.count(t => t != -1)
    }else{
      val res = tryFindChain(v, used, mt)
      goCicle(res._2, res._3, v+1, border)
    }
  }

  import scala.math
  val parts  = scala.io.StdIn.readLine().split(" ")
  val (m, n, k) = (parts(0).toInt, parts(1).toInt, parts(2).toInt)
  val read = readToMap(Map[Int, Vector[Int]](),Map[Int, Vector[Int]](), 1, k)
  val g = read._1
  val greverse = read._2
  val used = HashSet[Int]()
  val mt1 = Vector.fill(n)(-1)
  val res1 = goCicle(used, mt1, 1, n)
  val mt2 = Vector.fill(m)(-1)
  val res2 = goCicle(used, mt2, 1, m)
  println(m + n - math.max(res1, res2))
}

