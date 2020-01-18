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

class Sphere(val transform: Matrix, val material: Material, val shadow: Boolean)
    extends SpaceObject {
  // Note origin and radius always assumed as unit sphere
  // Use setTransform for transformations
  type T = Sphere

  def constructor(t: Matrix, m: Material, s: Boolean): T = new Sphere(t, m, s)

  final override def equals(that: Any): Boolean = {
    that match {
      case that: Sphere => transform === that.transform && material === that.material
      case _            => false
    }
  }

  final override def hashCode: Int = (transform, material).##

  def localIntersect(r: Ray): Seq[Intersection] = {
    val (discriminant, a, b): (Double, Double, Double) = getDiscriminant(r)
    discriminant match {
      case x if x < 0 => List()
      case _ =>
        List(-b - math.sqrt(discriminant), -b + math.sqrt(discriminant))
          .map(_ / (2 * a))
          .map((t: Double) => new Intersection(t, this))
    }
  }

  def getDiscriminant(r: Ray): (Double, Double, Double) = {
    val sphere_to_ray: RTTuple = r.origin - Point(0, 0, 0)

    val a: Double = r.direction dot r.direction
    val b: Double = 2 * (r.direction dot sphere_to_ray)
    val c: Double = (sphere_to_ray dot sphere_to_ray) - 1

    ((b * b) - 4 * a * c, a, b)
  }

  def localNormalAt(p: RTTuple): RTTuple = {
    (p - Point(0, 0, 0))
  }

  def bounds: (RTTuple, RTTuple) = {
    (Point(-1, -1, -1), Point(1, 1, 1))
  }
}

object Sphere {
  def unitSphere(): Sphere =
    new Sphere(Matrix.getIdentityMatrix(4), Material.defaultMaterial(), true)
  def glassSphere(): Sphere =
    new Sphere(Matrix.getIdentityMatrix(4),
               Material.defaultMaterial().setTransparency(1.0).setRefractiveIndex(1.5),
               false)
}
