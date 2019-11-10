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

class Sphere(val transform: Matrix, val material: Material) extends SpaceObject {
  // Note origin and radius always assumed as unit sphere
  // Use setTransform for transformations
  type T = Sphere
  def constructor(t: Matrix, m: Material): T = new Sphere(t, m)

  final override def equals(that: Any): Boolean = {
    that match {
      case that: Sphere => transform === that.transform && material === that.material
      case _ => false
    }
  }

  final def ===(that: Sphere): Boolean = {
    transform === that.transform && material === that.material
  }

  final override def hashCode: Int = (transform, material).##


  def localIntersect(r: Ray): Seq[Intersection] = {
    val (discriminant, a, b): (Double, Double, Double) = getDiscriminant(r)
    discriminant match {
      case x if x < 0 => List()
      case _ => List(-b - math.sqrt(discriminant), -b + math.sqrt(discriminant)).map(_ / (2 * a)).map(
        (t: Double) => new Intersection(t, this))
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

}

object Sphere {
  def unitSphere(): Sphere = new Sphere(Matrix.getIdentityMatrix(4), Material.defaultMaterial())
}

// TODO: Move me
abstract class SpaceObject() {
  type T <: SpaceObject
  val material: Material
  val transform: Matrix

  def constructor(t: Matrix, m: Material): T

  def localNormalAt(p: RTTuple): RTTuple
  def normalAt(p: RTTuple): RTTuple = {
    val localPoint: RTTuple = transform.inverse.tupleMult(p)

    transform.inverse.transpose.tupleMult(localNormalAt(localPoint)).forceVector().normalise()
  }

  def localIntersect(r: Ray): Seq[Intersection]
  def intersect(r: Ray): Seq[Intersection] = {
    localIntersect(r.transform(transform.inverse))
  }

  def setMaterial(m: Material): T = constructor(transform, m)

  def setTransform(m: Matrix): T = constructor(m, material)
}