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

class Cylinder(val transform: Matrix, val material: Material) extends SpaceObject {
  type T = Cylinder

  def constructor(t: Matrix, m: Material): T = new Cylinder(t, m)

  final override def equals(that: Any): Boolean = {
    that match {
      case that: Cylinder => transform === that.transform && material === that.material
      case _              => false
    }
  }

  final override def hashCode: Int = (transform, material).##

  def localIntersect(r: Ray): Seq[Intersection] = {
    val a: Double = r.direction.x * r.direction.x + r.direction.z * r.direction.z
    if (doubleEq(a, 0)) List()
    else {

      val b: Double    = 2 * r.origin.x * r.direction.x + 2 * r.origin.z * r.direction.z
      val c: Double    = r.origin.x * r.origin.x + r.origin.z * r.origin.z - 1
      val disc: Double = (b * b) - 4 * a * c
      if (disc < 0) List()
      else {
        List(Intersection((-b - math.sqrt(disc)) / (2 * a), this),
             Intersection((-b + math.sqrt(disc)) / (2 * a), this))
      }
    }
  }

  def localNormalAt(p: RTTuple): RTTuple = {
    Vector(p.x, 0, p.z)
  }
}

object Cylinder {
  def apply(): Cylinder =
    new Cylinder(Matrix.getIdentityMatrix(4), Material.defaultMaterial())
}
