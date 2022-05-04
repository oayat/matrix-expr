import scala.collection.mutable.ArrayBuffer

object MatrixMultData {
  var n = 0 // number of matrices to multiply
  val mnum = 4 // number of matrices to multiply
  var matrices = ArrayBuffer[Array[Array[Double]]]() // table of matrices
  var dims = ArrayBuffer[Array[Long]]() // table of dimensions of matrices
  var N = Array[Array[Long]]()

  def OptimalMatrixMult(): Matrix = {
    /*println("\nDimensions of " + n + " matrices to analyze:")
    for (i <- 1 to n) {
      println("Matrix "+ i + ": " + dims(i)(0) + " x " + dims(i)(1))
    }*/

    N = Array.ofDim[Long](n + 1, n + 1)
    var j = 0
    for (len <- 2 to n) {
      for (i <- 1 to n - len + 1) {
        j = i + len - 1
        val lst = for (k <- i to j - 1) yield dims(i)(0) * dims(k)(1) * dims(j)(1) + N(i)(k) + N(k + 1)(j)
        if (lst.length > 0) N(i)(j) = lst.min
      }
    }

    /*println("Table N: ")
    N.map(_.mkString(" ")).foreach(println)
    println*/

    val resData = backtrack_matrix_mult(1, n)
    new Matrix(resData.length, resData(0).length, resData)
  }

  def backtrack_matrix_mult(i: Int, j: Int): Array[Array[Double]] =
  {
    if (i == j)
    {
      matrices(i)
    }
    else {
           var split = 0
           for (k <- i to j - 1)
           {
             val candidate = dims(i)(0) * dims(k)(1) * dims(j)(1) + N(i)(k) + N(k + 1)(j)
             if (candidate == N(i)(j)) split = k
           }
           val left = backtrack_matrix_mult(i, split)
           val right = backtrack_matrix_mult(split + 1, j)
           matrix_mult(left, right)
    }
  }

  def matrix_mult(left: Array[Array[Double]], right: Array[Array[Double]]): Array[Array[Double]] =
  {
    val rows1 = left.length
    val cols1 = left(0).length

    val rows2 = right.length
    val cols2 = right(0).length

    val resMatrix = Array.ofDim[Double](rows1, cols2)

    for (i <- 0 until rows1)
      for (j <- 0 until cols2)
      {
        resMatrix(i)(j) = 0.0
        for (k <- 0 until rows2)
          resMatrix(i)(j) += left(i)(k) * right(k)(j)
      }

    //println("Multiplied (" + rows1 + " x " + cols1 + ") and (" + rows2 + " x " + cols2 + ")")

    resMatrix
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
    val resData = Array.ofDim[Double](2, 2)

    if (MatrixMultData.n == 0)
    {
      MatrixMultData.matrices.clear()

      MatrixMultData.matrices.append(this.data)
      MatrixMultData.dims.append(Array(rows, cols))

      MatrixMultData.n += 1
      MatrixMultData.matrices.append(this.data)
      MatrixMultData.dims.append(Array(rows, cols))
    }
    MatrixMultData.n += 1
    MatrixMultData.matrices.append(that.data)
    MatrixMultData.dims.append(Array(rows2, cols2))

    if (MatrixMultData.n == MatrixMultData.mnum)
    {
      MatrixMultData.OptimalMatrixMult()
    }
    else new Matrix(2, 2, resData)
  }
}

object MatrixExpr extends MatrixMultData {

  def Matrix2(rows: Int, cols: Int, fillValue: Double): Matrix = {
   val M = new Matrix(rows, cols, Array.ofDim[Double](rows, cols))
   M.init(fillValue)
   M
  }

  def CompareMatrices(m1: Matrix, m2: Matrix) =
  {
     val rows1 = m1.data.length
     val cols1 = m1.data(0).length
     val rows2 = m2.data.length
     val cols2 = m2.data(0).length
     var done = false

     //println("Comparing matrices.")
     if (rows1 != rows2) {
       println("Number of rows do not match.")
       done = true
     }
     if (cols1 != cols2) {
       println("Number of columns do not match.")
       done = true
     }

     if ((rows1 == rows2) && (cols1 == cols2))
     {
       for (i <- 0 until rows1 ; if !done)
         for (j <- 0 until cols1 ; if !done) {
           if (m1.data(i)(j) != m2.data(i)(j))
           {
             done = true
             println("m1.data(" + i + ")(" + j + ") != " + "m2.data(" + i + ")(" + j + ")")
             println(m1.data(i)(j) + " != " +  m2.data(i)(j))
           }
         }
     }
     if (done == false) {
       //println("OK.")
     }
  }

  def main(args: Array[String]) = {
    println("Matrix expression experiments.\n")

    val A = Matrix2(1200, 900, 1) //(2000, 1500, 1)
    val B = Matrix2(900, 90, 1) //(1500, 150, 1)
    val C = Matrix2(90, 1500, 1) //(150, 2500, 1)
    val D = Matrix2(1500, 1200, 1) //(2500, 2000, 1)

    println("Calculating A * B * C * D...")
    var t1 = System.nanoTime
    val expr1 = A * B * C * D
    val duration1 = (System.nanoTime - t1) / 1e9d
    println("Time for calculating A * B * C * D: " + duration1 + " seconds.\n")

    println("Calculating A * B * C * D with optimization...")
    var t2 = System.nanoTime
    var expr2 = A ** B ** C ** D
    val duration2 = (System.nanoTime - t2) / 1e9d
    println("Time for calculating A * B * C * D: " + duration2 + " seconds.\n")

    CompareMatrices(expr1, expr2)

    println("Calculating (A * B) * (C * D)...")
    val t3 = System.nanoTime
    val expr3 = (A * B) * (C * D)
    val duration3 = (System.nanoTime - t3) / 1e9d
    println("Time for calculating (A * B) * (C * D): " + duration3 + " seconds.\n")

    CompareMatrices(expr2, expr3)

    println("t1/t2: " + duration1/duration2 + ".")
    println("t1/t3: " + duration1/duration3 + ".")
    println("t2/t3: " + duration2/duration3 + ".")
  }
}
