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

class Plane(val transform: Matrix, val material: Material) extends SpaceObject {
  type T = Plane

  def constructor(t: Matrix, m: Material): T = new Plane(t, m)

  final override def equals(that: Any): Boolean = {
    that match {
      case that: Plane => transform === that.transform && material === that.material
      case _ => false
    }
  }

  final def ===(that: Plane): Boolean = {
    transform === that.transform && material === that.material
  }

  final override def hashCode: Int = (transform, material).##


  def localIntersect(r: Ray): Seq[Intersection] = {
    if (math.abs(r.direction.y) < EPSILON) (List[Intersection]()) else {
      val t: Double = -r.origin.y / r.direction.y
      List(new Intersection(t, this))
    }
  }


  def localNormalAt(p: RTTuple): RTTuple = {
    Vector(0, 1, 0)
  }
}


object Plane {
  def apply(): Plane = new Plane(Matrix.getIdentityMatrix(4), Material.defaultMaterial())
}