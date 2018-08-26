package tool

object Tool {

  //implicit def toMyExpr( target: Any ) = new MyExpr(target)
  //implicit def intToRational( i: Int ) = new Rational(i)

  type Throwable = java.lang.Throwable
  type Exception = java.lang.Exception
  type Error = java.lang.Error
  type Seq[+A] = scala.collection.Seq[A]
  val Seq = scala.collection.Seq

  def showVersion(note: String = "Comcx") = {
    println("    ________ ___   / /  ___ ")
    println("   / __/ __// _ | / /  / _ |")
    println(" __\\ \\/ /__/ __ |/ /__/ __ |")
    println("/____/\\___/_/ |_/____/_/ | | "+"  by "+note)
    println("                         |/  ")
    hrLine()
  }

  def scalaLogo(note: String): String = {
    "    ________ ___   / /  ___ " + "\r\n" +
      "   / __/ __// _ | / /  / _ |" + "\n" +
      " __\\ \\/ /__/ __ |/ /__/ __ |" + "\n" +
      "/____/\\___/_/ |_/____/_/ | | "+"  by "+ note + "\n" +
      "                         |/  "
  }

  def Timer = new java.util.Date ()

  def hrLine(length: Int = 99) = {
    var outLine: String = "-"
    for( i <- 0 to length )  outLine += "-"
    outLine
  }

  def twice(condition: => Boolean)(block: =>Unit) {
    if(condition){
      for(i <- 0 to 1)  block
    }
  }

  def makeList[A](theRange: Range, makeWay: => A) =
    for(i <- List.range[Int](theRange.start , theRange.end))  yield makeWay



  type CriterionMsg [A] = (String, () => A, Boolean, String)
  def benchmark [A] (title: String, f: () => A,
                     show: Boolean = false, precision: String = "ns"): CriterionMsg [A] =
    (title, f, show, precision)

  def criterion [A] (msg: List [CriterionMsg [A]]): Unit = {

    def useTime [A] (f: () => A, precision: String = "ns"): (Long, A) = {

      def switch (precision: String): Long = precision match {
        case "ms" => Tool.Timer.getTime
        case "ns" => System.nanoTime()
      }

      val t_start = switch (precision)
      val res: A = f ()
      val t_end = switch (precision)
      (t_end - t_start, res)
    }

    msg.foreach { e =>
      println ("-> " + e._1 + ":")
      val res = useTime (e._2, e._4)
      print ("\t")
      print (if (e._3) res._2 else ""); print (if (e._3) "\n" else "")
      println ("\ttime used: %d %s".format (res._1, e._4))
    }

  }

}
