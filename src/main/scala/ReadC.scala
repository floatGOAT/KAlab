import scala.annotation.tailrec

object ReadC extends App{
  @tailrec
  def readToMap(g:Map[Int, Vector[(Int, Int)]],  count:Int, border:Int):
  Map[Int, Vector[(Int, Int)]] ={
    if (count>border)
      g
    else {
      val p  = scala.io.StdIn.readLine().split(" ")
      val (a, b, c) = (p(0).toInt, p(1).toInt, p(2).toInt)
      g.get(a) match {
        case Some(value) =>
          readToMap(g + (a -> (value :+ (b, c))), count+1, border)
        case None =>
          readToMap(g + (a -> Vector((b, c))), count+1, border)
      }
    }
  }

  val parts: Array[String] = scala.io.StdIn.readLine().split(" ")
  val (n, m) = (parts(0).toInt, parts(1).toInt)
  val g: Map[Int, Vector[(Int, Int)]] = readToMap(Map[Int, Vector[(Int, Int)]](), 1, m)
  val partsST: Array[String] = scala.io.StdIn.readLine().split(" ")
  val (s, t) = (partsST(0).toInt, partsST(1).toInt)
  println(s"$m hey $n")
  println(g)
  println(s"$s wow $t")
}
