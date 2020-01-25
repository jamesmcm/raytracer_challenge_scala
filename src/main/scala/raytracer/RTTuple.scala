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

class RTTuple(val x: Double, val y: Double, val z: Double, val w: Double) {

  def toTuple: (Double, Double, Double, Double) = (x, y, z, w)

  def toArray: Array[Double] = Array(x, y, z, w)

  final override def equals(that: Any): Boolean = {
    that match {
      case that: RTTuple =>
        doubleEq(that.x, x) && doubleEq(that.y, y) && doubleEq(that.z, z) && (that.w == w)
      case _ => false
    }
  }

  final def ===(that: RTTuple): Boolean = {
    doubleEq(that.x, x) && doubleEq(that.y, y) && doubleEq(that.z, z) && (that.w == w)
  }

  final override def hashCode: Int = (x, y, z, w).##

  def +(that: RTTuple): RTTuple = new RTTuple(x + that.x, y + that.y, z + that.z, w + that.w)

  def -(that: RTTuple): RTTuple = new RTTuple(x - that.x, y - that.y, z - that.z, w - that.w)

  def *(that: Double): RTTuple = new RTTuple(x * that, y * that, z * that, w * that)

  def /(that: Double): RTTuple = new RTTuple(x / that, y / that, z / that, w / that)

  def negate(): RTTuple = new RTTuple(-x, -y, -z, -w)

  def magnitude(): Double = Math.sqrt(x * x + y * y + z * z + w * w)

  def normalise(): RTTuple = this / this.magnitude()

  def dot(that: RTTuple): Double = x * that.x + y * that.y + z * that.z + w * that.w

  def cross(that: RTTuple): RTTuple =
    Vector(y * that.z - z * that.y, z * that.x - x * that.z, x * that.y - y * that.x)

  def forceVector(): RTTuple = Vector(x, y, z)

  def forcePoint(): RTTuple = Point(x, y, z)

  def reflect(normal: RTTuple): RTTuple = this - normal * 2 * this.dot(normal)

  final override def toString: String = {
    "(" + x.toString + ", " + y.toString + ", " + z.toString + ", " + w.toString + ")"
  }

}

object Vector {
  def apply(x: Double, y: Double, z: Double): RTTuple = new RTTuple(x, y, z, 0)
}

object Point {
  def apply(x: Double, y: Double, z: Double): RTTuple = new RTTuple(x, y, z, 1)
}
