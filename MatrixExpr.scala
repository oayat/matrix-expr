import scala.collection.mutable.ArrayBuffer

object MatrixMultData {
  var n = 0
  var matrices = ArrayBuffer[Array[Array[Double]]]()
  var dims = ArrayBuffer[Array[Int]]()

  def OptimalMatrixMult(): Matrix = {
    var N = Array.ofDim[Long](n+1, n+1)

    /*println("\nDimensions of " + (n+1) + " matrices to analyze:")
    for (i <- 0 to n) {
      println("Matrix "+ i + ": " + dims(i)(0) + " x " + dims(i)(1))
    }*/

    var j = 0
    var tmp: Long = 0
    for (len <- 1 to n) {
      for (i <- 0 to n - len) {
        j = i + len - 1
        val lst = for (k <- i to j - 1) yield dims(i)(0) * dims(k)(1) * dims(j)(1) + N(i)(k) + N(k + 1)(j)
        if (lst.length > 0) N(i)(j) = lst.min
      }
    }

    println("Table N: ")
    N.map(_.mkString(" ")).foreach(println)
    println

    val resData = Array.ofDim[Double](2, 2) // !!!
    new Matrix(2, 2, resData)
  }
}
trait MatrixMultData

class Matrix(rows: Int, cols: Int, val data: Array[Array[Double]]) extends MatrixMultData {

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

    for (i <- 0 until rows)
      for (j <- 0 until cols2)
      {
        resData(i)(j) = 0.0
        for (k <- 0 until rows2)
          resData(i)(j) += this.data(i)(k) * that.data(k)(j)
      }
    new Matrix(rows, cols, resData)
  }

  def **(that: Matrix) = {
    val rows2 = that.data.length
    val cols2 = that.data(0).length
    val resData = Array.ofDim[Double](rows, cols2)

    if (MatrixMultData.n == 0)
    {
      MatrixMultData.matrices.append(this.data)
      MatrixMultData.dims.append(Array(rows, cols))
    }
    MatrixMultData.n += 1
    MatrixMultData.matrices.append(that.data)
    MatrixMultData.dims.append(Array(rows2, cols2))

    new Matrix(rows, cols, resData)
  }

}

object MatrixExpr extends MatrixMultData {

  def Matrix2(rows: Int, cols: Int, fillValue: Double): Matrix = {
   val M = new Matrix(rows, cols, Array.ofDim[Double](rows, cols))
   M.init(fillValue)
   M
  }

  def main(args: Array[String]) = {
    println("Matrix expression experiments.\n")

    val A = Matrix2(400, 300, 1000.005)
    val B = Matrix2(300, 30, 1000.005)
    val C = Matrix2(30, 500, 1000.005)
    val D = Matrix2(500, 400, 1000.005)

    println("Calculating expression 1.\n")
    MatrixMultData.matrices.clear()
    var t1 = System.nanoTime
    val Expr1 = A ** B ** C ** D
    MatrixMultData.OptimalMatrixMult()

    val duration1 = (System.nanoTime - t1) / 1e9d

    /*MatrixMultData.n = 0E
    t1 = System.nanoTime
    println("Calculating expression 2.\n")
    val Expr2 = (A * B) * (C * D)
    val duration2 = (System.nanoTime - t1) / 1e9d
    println("n = " + MatrixMultData.n)*/

    println("Time for calculating expr 1: " + duration1 + " seconds.")
    //println("Time for calculating expr 2: " + duration2 + " seconds.")
    //println("Speedup = Time1/Time2: " + duration1/duration2 + ".")
  }
}
