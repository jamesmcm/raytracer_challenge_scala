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

class RTTuple(val x: Double, val y: Double, val z: Double, val w: Double){

  def toTuple: (Double, Double, Double, Double) = (x, y, z, w)

  final override def equals(that: Any): Boolean = {
    that match {
      case that: RTTuple => doubleEq(that.x, x) && doubleEq(that.y, y) && doubleEq(that.z, z) && (that.w === w)
      case _ => false
    }
  }

  final override def hashCode: Int = (x, y, z, w).##

  def +(that: RTTuple): RTTuple = new RTTuple(x + that.x, y + that.y, z + that.z, w + that.w)

  def -(that: RTTuple): RTTuple = new RTTuple(x - that.x, y - that.y, z - that.z, w - that.w)

  def negate(): RTTuple = new RTTuple(-x, -y, -z, -w)

}

object Vector {
  def apply(x: Double, y: Double, z: Double): RTTuple = new RTTuple(x, y, z, 0)
}

object Point {
  def apply(x: Double, y: Double, z: Double): RTTuple = new RTTuple(x, y, z, 1)
}
