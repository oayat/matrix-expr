class Matrix(rows: Int, cols: Int, val data: Array[Array[Double]]){
  /*private var n = 4
  private var matrices = Array.ofDim[Matrix](4, 2)
  private var dims = Array.ofDim[Int](4, 2)*/

  def init(value: Double) {
    for (i <- 0 until rows)
      for (j <- 0 until cols)
        data(i)(j) = value
  }

  def print(s: String) {
    println(s)
    data.map(_.mkString(" ")).foreach(println)
    println
  }

  def +(that: Matrix) = {
    /*if ((rows != that.data.size) || (cols != that.data.size))
    {
      println("Non conformable matrices in function +")
    }*/

    val resData = Array.ofDim[Double](rows, cols)

    for (i <- 0 until rows)
      for (j <- 0 until cols)
        resData(i)(j) = this.data(i)(j) + that.data(i)(j)

    new Matrix(rows, cols, resData)
  }

  def *(that: Matrix) = {
    val rows2 = that.data.length
    val cols2 = that.data(0).length
    val resData = Array.ofDim[Double](rows, cols2)

    println("this matrix: " + rows + " x " + cols)
    println("that matrix: " + rows2 + " x " + cols2 + "\n")

    for (i <- 0 until rows)
      for (j <- 0 until cols2)
      {
        resData(i)(j) = 0.0
        for (k <- 0 until rows2)
          resData(i)(j) += this.data(i)(k) * that.data(k)(j)
      }
      new Matrix(rows, cols, resData)
  }
}

object MatrixExpr {

  def Matrix2(rows: Int, cols: Int, fillValue: Double): Matrix = {
   val M = new Matrix(rows, cols, Array.ofDim[Double](rows, cols))
   M.init(fillValue)
   return M
  }

  def main(args: Array[String]) = {
    println("Matrix expression experiments.\n")

    val A = Matrix2(400, 300, 1000.005)
    val B = Matrix2(300, 30, 1000.005)
    val C = Matrix2(30, 500, 1000.005)
    val D = Matrix2(500, 400, 1000.005)

    println("Calculating expression 1.\n")
    var t1 = System.nanoTime
    val Expr1 = A * B * C * D
    val duration1 = (System.nanoTime - t1) / 1e9d

    t1 = System.nanoTime
    println("Calculating expression 2.\n")
    val Expr2 = (A * B) * (C * D)
    val duration2 = (System.nanoTime - t1) / 1e9d

    println("Time for calculating expr 1: " + duration1 + " seconds.")
    println("Time for calculating expr 2: " + duration2 + " seconds.")
    println("Speedup = Time1/Time2: " + duration1/duration2 + ".")
    println("\nfin")
  }

}
