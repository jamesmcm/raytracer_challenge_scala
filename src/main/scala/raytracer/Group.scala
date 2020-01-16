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

class Group(val transform: Matrix,
            val material: Material,
            val shadow: Boolean,
            val objs: List[SpaceObject])
    extends SpaceObject {
  type T = Group

  def constructor(t: Matrix, m: Material, s: Boolean): T = new Group(t, m, s, objs)
  def constructor(t: Matrix, m: Material, s: Boolean, objs: List[SpaceObject]): T = {
    val newgroup: Group = new Group(t, m, s, objs)
    objs.foreach((x: SpaceObject) => x.setParent(newgroup))
    newgroup
  }

  final override def equals(that: Any): Boolean = {
    that match {
      case that: Group =>
        transform === that.transform && material === that.material && objs == that.objs
      case _ => false
    }
  }

  final override def hashCode: Int = (transform, material, objs).##

  def isEmpty: Boolean = {
    objs.isEmpty
  }

  def setChildren(o: List[SpaceObject]): Group = {
    constructor(transform, material, shadow, o)
  }

  override final def setTransform(m: Matrix): Group = {
    constructor(m, material, shadow, objs)
  }

  def localIntersect(r: Ray): Seq[Intersection] = {
    objs.flatMap((x: SpaceObject) => x.intersect(r)).sortBy((z: Intersection) => z.t)
  }

  def localNormalAt(p: RTTuple): RTTuple = {
    throw new UnsupportedOperationException("Tried to call localNormalAt() on group")
  }

  def addChild(c: SpaceObject): Group = {
    val newgroup: Group = constructor(transform, material, shadow, objs :+ c)
    c.setParent(newgroup)
    newgroup
  }

  def contains(c: SpaceObject): Boolean = {
    objs.contains(c)
  }
}

object Group {
  def apply(): Group =
    new Group(Matrix.getIdentityMatrix(4), Material.defaultMaterial(), true, List())
}
