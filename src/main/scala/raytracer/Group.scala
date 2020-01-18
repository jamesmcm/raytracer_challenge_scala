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
            val objs: List[SpaceObject],
            val stored_bounds: Option[(RTTuple, RTTuple)]
)
    extends SpaceObject {
  type T = Group

  // TODO: Override setMaterial - apply to all children

  def constructor(t: Matrix, m: Material, s: Boolean): T = new Group(t, m, s, objs, None)
  def constructor(t: Matrix,
                  m: Material,
                  s: Boolean,
                  objs: List[SpaceObject],
                  b: Option[(RTTuple, RTTuple)]): T = {
    val newgroup: Group = new Group(t, m, s, objs, b)
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
    constructor(transform, material, shadow, o, stored_bounds)
  }

  override final def setTransform(m: Matrix): Group = {
    constructor(m, material, shadow, objs, stored_bounds)
  }

  def localIntersect(r: Ray): Seq[Intersection] = {
    if (stored_bounds.isDefined) {
      if (boundsIntersect(r)) {
        objs.flatMap((x: SpaceObject) => x.intersect(r)).sortBy((z: Intersection) => z.t)
      } else { List() }
    } else {
      objs.flatMap((x: SpaceObject) => x.intersect(r)).sortBy((z: Intersection) => z.t)
    }
  }

  def localNormalAt(p: RTTuple): RTTuple = {
    throw new UnsupportedOperationException("Tried to call localNormalAt() on group")
  }

  def addChild(c: SpaceObject): Group = {
    val newgroup: Group = constructor(transform, material, shadow, objs :+ c, stored_bounds)
    c.setParent(newgroup)
    newgroup
  }

  def contains(c: SpaceObject): Boolean = {
    objs.contains(c)
  }

  def setBounds(): Group = {
    constructor(transform, material, shadow, objs, Some(bounds))
  }

  def bounds: (RTTuple, RTTuple) = {
    val vertices: List[RTTuple] = objs.flatMap((s: SpaceObject) => {
        val x = s.bounds;
        List(
          Point(x._1.x, x._1.y, x._1.z),
          Point(x._2.x, x._1.y, x._1.z),
          Point(x._1.x, x._2.y, x._1.z),
          Point(x._2.x, x._2.y, x._1.z),
          Point(x._1.x, x._1.y, x._2.z),
          Point(x._2.x, x._1.y, x._2.z),
          Point(x._1.x, x._2.y, x._2.z),
          Point(x._2.x, x._2.y, x._2.z),
        ).map((p: RTTuple) => s.transform.tupleMult(p))
      }) // .map((x: RTTuple) => transform.tupleMult(x))

    val xs2: List[Double] = vertices.map((x: RTTuple) => x.x )
    val ys2: List[Double] = vertices.map((x: RTTuple) => x.y )
    val zs2: List[Double] = vertices.map((x: RTTuple) => x.z )

    (Point(xs2.min, ys2.min, zs2.min), Point(xs2.max, ys2.max, zs2.max))
  }

  def boundsIntersect(r: Ray): Boolean = {
    val xpair: (Double, Double) = Group.checkAxis(r.origin.x, r.direction.x, stored_bounds.get._1.x, stored_bounds.get._2.x)
    val ypair: (Double, Double) = Group.checkAxis(r.origin.y, r.direction.y, stored_bounds.get._1.y, stored_bounds.get._2.y)
    val zpair: (Double, Double) = Group.checkAxis(r.origin.z, r.direction.z, stored_bounds.get._1.z, stored_bounds.get._2.z)

    val tmin: Double =
      List(xpair._1, ypair._1, zpair._1).foldLeft(Double.NegativeInfinity)((x: Double, y: Double) =>
        if (x >= y) x else y)
    val tmax: Double =
      List(xpair._2, ypair._2, zpair._2).foldLeft(Double.PositiveInfinity)((x: Double, y: Double) =>
        if (x <= y) x else y)

    if (tmin > tmax) false else true
  }
}

object Group {
  def apply(): Group =
    new Group(Matrix.getIdentityMatrix(4), Material.defaultMaterial(), true, List(), None)

  def checkAxis(origin: Double, direction: Double, minaxis: Double, maxaxis: Double): (Double, Double) = {
    val tmin_numerator: Double = minaxis - origin
    val tmax_numerator: Double = maxaxis - origin

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
}
