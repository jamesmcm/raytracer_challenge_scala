// Copyright (C) 2019 James McMurray
//
// raytracer_challenge is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// raytracer_challenge is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with raytracer_challenge  If not, see <http://www.gnu.org/licenses/>.

package raytracer

import cats.implicits._

import scala.reflect.ClassTag

class Matrix (val m: Array[Array[Double]]) {

  def apply(row: Int, column: Int): Double = {
    // Allow us to index with M(row)(column)
    m(row)(column)
  }

  final override def equals(that: Any): Boolean = {
    that match {
      case that: Matrix if that.m.length === m.length && that.m(0).length === m(0).length => Matrix.mapMatrix(
        (this zip that),
        ((x: (Double, Double)) => doubleEq(x._1, x._2)))
        .map((z: Array[Boolean]) => z.forall(identity)).reduce(_ && _)

      // .map((x: Array[(Double, Double)]) => x.map((y: (Double, Double)) => doubleEq(y._1, y._2)))
      // .map((z: Array[Boolean]) => z.forall(identity)).reduce(_ && _)
      case _ => false
    }
  }

  def zip(that: Matrix): Array[Array[(Double, Double)]] = {
    Matrix.zipMatrix(m, that.m)
    // m.zip(that.m).map((x: (Array[Double], Array[Double])) => x._1 zip x._2)
  }

  def row(i: Int): Array[Double] = {
    m(i)
  }

  def col(i: Int): Array[Double] = {
    m.map((x: Array[Double]) => x(i))
  }

  def *(that: Matrix): Matrix = {
    new Matrix(
      Matrix.mapMatrix(
        Matrix.zipMatrix(
          m.zipWithIndex.map((x: (Array[Double], Int)) => x._1.map((y: Double) => row(x._2))),
          that.m.map((x: Array[Double]) => x.zipWithIndex.map((y: (Double, Int)) => that.col(y._2)))
        ),
        ((x: (Array[Double], Array[Double])) => Matrix.dotArray(x._1, x._2)))
    )
  }

  def tupleMult(that: RTTuple): RTTuple = {
    // TODO: Break up matrix multiplication function so we don't need to instantiate Matrix here
    val args: Array[Double] = (this * (new Matrix(Array(Array(that.x), Array(that.y), Array(that.z), Array(that.w))))).m.flatten
    new RTTuple(args(0), args(1), args(2), args(3))
  }

  def transpose: Matrix = {
    new Matrix(m(0).indices.map(col).toArray)
  }

  def determinant: Double = {
    m.length match {
      case 2 => m(0)(0) * m(1)(1) - m(0)(1) * m(1)(0)
      case _ =>  m(0).indices.map((x: Int) => m(0)(x) * cofactor(0, x)).sum
    }
  }

  def submatrix(row: Int, col: Int): Matrix = {
    // Delete row and col
    new Matrix(
      ((x: Array[Array[Double]]) => x.take(row) ++ x.drop(row + 1)) (
        m.map((x: Array[Double]) => x.take(col) ++ x.drop(col + 1)))
    )
  }

  def minor(row: Int, col: Int): Double = {
    submatrix(row, col).determinant
  }

  def cofactor(row: Int, col: Int): Double = {
    row + col match {
      case x if x % 2 === 0 => minor(row, col)
      case _ => -1 * minor(row, col)
    }
  }

  def inverse: Matrix = {
    // TODO: Avoid creating two Matrix objects here
    new Matrix(
      m.zipWithIndex.map((x: (Array[Double], Int)) => x._1.indices.map(
      (y: Int) => cofactor(x._2, y) / determinant
    ).toArray
    )
    ).transpose
  }

  def isInvertible: Boolean = !(determinant === 0)

  final override def hashCode: Int = m.##

}


object Matrix {
  def matrixFromString(s: String): Matrix = {
    new Matrix(s.split("\n").map(_.filter(_ =!= ' ').split("\\|").filter(_ =!= "|").map(_.toDouble)))
  }

  def dotArray(a: Array[Double], b: Array[Double]): Double = {
    (a zip b).map((x: (Double, Double)) => x._1 * x._2).sum
  }

  def zipMatrix[A](a: Array[Array[A]], b: Array[Array[A]]): Array[Array[(A, A)]] = {
    a.zip(b).map((x: (Array[A], Array[A])) => x._1 zip x._2)
  }

  def mapMatrix[A, B: ClassTag](a: Array[Array[A]], fun: A => B): Array[Array[B]] = {
    a.map((x: Array[A]) => x.map((y: A) => fun(y)))
  }

  def getIdentityMatrix(size: Int): Matrix = {
    new Matrix(
      Array.fill(size)(Array.fill(size)(0:Double)).zipWithIndex.map(
      (x: (Array[Double], Int)) => x._1.zipWithIndex.map((y: (Double, Int)) => if (y._2 === x._2) 1:Double else 0:Double))
    )
  }
}