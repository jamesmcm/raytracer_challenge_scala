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

class Cube(val transform: Matrix, val material: Material, val shadow: Boolean) extends SpaceObject {
  type T = Cube

  def constructor(t: Matrix, m: Material, s: Boolean): T = new Cube(t, m, s)

  final override def equals(that: Any): Boolean = {
    that match {
      case that: Cube => transform === that.transform && material === that.material
      case _          => false
    }
  }

  final override def hashCode: Int = (transform, material).##

  def localIntersect(r: Ray): Seq[Intersection] = {
    val xpair: (Double, Double) = Cube.checkAxis(r.origin.x, r.direction.x)
    val ypair: (Double, Double) = Cube.checkAxis(r.origin.y, r.direction.y)
    val zpair: (Double, Double) = Cube.checkAxis(r.origin.z, r.direction.z)

    val tmin: Double =
      List(xpair._1, ypair._1, zpair._1).foldLeft(Double.NegativeInfinity)((x: Double, y: Double) =>
        if (x >= y) x else y)
    val tmax: Double =
      List(xpair._2, ypair._2, zpair._2).foldLeft(Double.PositiveInfinity)((x: Double, y: Double) =>
        if (x <= y) x else y)

    if (tmin > tmax) List() else List(Intersection(tmin, this), Intersection(tmax, this))
  }

  def localNormalAt(p: RTTuple): RTTuple = {
    val maxc: Double = List(math.abs(p.x), math.abs(p.y), math.abs(p.z)).foldLeft(Double.NegativeInfinity)((x: Double, y:Double) => if (x>= y) x else y )

    maxc match {
      case z if z === math.abs(p.x) => Vector(p.x, 0, 0)
      case z if z === math.abs(p.y) => Vector(0, p.y, 0)
      case _ => Vector(0, 0, p.z)
    }
  }
}

object Cube {
  def checkAxis(origin: Double, direction: Double): (Double, Double) = {
    val tmin_numerator: Double = -1 - origin
    val tmax_numerator: Double = 1 - origin

    if (math.abs(direction) >= EPSILON) {
      leftSmaller(tmin_numerator / direction, tmax_numerator / direction)
    } else {
      leftSmaller(tmin_numerator * Double.PositiveInfinity,
                  tmax_numerator * Double.PositiveInfinity)
    }

  }

  def leftSmaller(t: (Double, Double)): (Double, Double) = {
    if (t._1 > t._2) (t._2, t._1) else t
  }

  def apply(): Cube =
    new Cube(Matrix.getIdentityMatrix(4), Material.defaultMaterial(), true)
}
