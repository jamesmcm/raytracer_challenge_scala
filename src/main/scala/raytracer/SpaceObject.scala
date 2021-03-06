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

import scala.annotation.tailrec

abstract class SpaceObject() {
  type T <: SpaceObject
  val material: Material
  val transform: Matrix
  val shadow: Boolean
  val transform_inverse: Matrix = transform.inverse
  def equals(that: Any): Boolean

  var parent: Option[SpaceObject] = None

  final def ===(that: SpaceObject): Boolean = {
    that match {
      case that: T =>
        transform === that.transform && material === that.material // TODO: Fix me - type erasure
      case _ => false
    }
  }

  def setParent(p: SpaceObject): Unit = {
    parent = Some(p)
  }

  def hashCode: Int

  def constructor(t: Matrix, m: Material, s: Boolean): T

  def bounds: (RTTuple, RTTuple) // Untransformed

  def localNormalAt(p: RTTuple, hit: Intersection): RTTuple

  def normalAt(p: RTTuple, hit: Intersection): RTTuple = {
    val localPoint: RTTuple = worldToObject(p)

    normalToWorld(localNormalAt(localPoint, hit))
  }

  def localIntersect(r: Ray): Seq[Intersection]

  def intersect(r: Ray): Seq[Intersection] = {
    localIntersect(r.transform(transform_inverse))
  }

  def setMaterial(m: Material): T = constructor(transform, m, shadow)

  def setShadow(b: Boolean): T = constructor(transform, material, b)

  def setTransform(m: Matrix): T = constructor(m, material, shadow)

  def worldToObject(p: RTTuple): RTTuple = {
    //TODO: Unbounded recursion - could overflow stack
    val p2: RTTuple = if (parent.isEmpty) p else parent.get.worldToObject(p)
    transform_inverse.tupleMult(p2)
  }
  @tailrec
  final def normalToWorld(n: RTTuple): RTTuple = {
    val x: RTTuple = transform_inverse.transpose.tupleMult(n).forceVector().normalise()
    if (parent.isEmpty) x else parent.get.normalToWorld(x)
  }
}
