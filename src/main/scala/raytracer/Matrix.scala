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

class Matrix (val m: Array[Array[Double]]){

  def apply(row: Int, column: Int): Double = {
    m(row)(column)
  }

  final override def equals(that: Any): Boolean = {
    that match {
      case that: Matrix if that.m.length === m.length && that.m(0).length === m(0).length => (this zip that)
        .map((x: Array[(Double, Double)]) => x.map((y: (Double, Double)) => doubleEq(y._1, y._2)))
        .map((z: Array[Boolean]) => z.forall(identity)).reduce(_ && _)
      case _ => false
    }
  }

  def zip(that: Matrix): Array[Array[(Double, Double)]] = {
    m.zip(that.m).map((x: (Array[Double], Array[Double])) => x._1 zip x._2)
  }

  final override def hashCode: Int = m.##

}


object Matrix {
  def matrixFromString(s: String): Matrix = {
    new Matrix(s.split("\n").map(_.filter(_ =!= ' ').split("\\|").filter(_ =!= "|").map(_.toDouble)))
  }
}