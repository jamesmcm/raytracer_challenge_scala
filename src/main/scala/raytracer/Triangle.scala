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

class Triangle(val transform: Matrix,
               val material: Material,
               val shadow: Boolean,
               val points: (RTTuple, RTTuple, RTTuple),
               val edges: (RTTuple, RTTuple),
               val normal: RTTuple)
    extends SpaceObject {
  type T = Triangle

  def constructor(t: Matrix, m: Material, s: Boolean): T = {
    new Triangle(t, m, s, points, edges, normal)
  }

  def constructor(t: Matrix, m: Material, s: Boolean, p: (RTTuple, RTTuple, RTTuple)): T = {
    val e1: RTTuple     = p._2 - p._1
    val e2: RTTuple     = p._3 - p._1
    val normal: RTTuple = e2.cross(e1).normalise()

    new Triangle(t, m, s, p, (e1, e2), normal)
  }

  final override def equals(that: Any): Boolean = {
    that match {
      case that: Triangle =>
        transform === that.transform && material === that.material && points == that.points
      case _ => false
    }
  }

  final override def hashCode: Int = (transform, material, points).##

  def localIntersect(r: Ray): Seq[Intersection] = {
    val dir_cross_e2: RTTuple = r.direction.cross(edges._2)
    val det: Double           = edges._1.dot(dir_cross_e2)
    if (det.abs < EPSILON) { List() } else {
      val f: Double             = 1.0 / det
      val p1_to_origin: RTTuple = r.origin - points._1
      val u: Double             = f * p1_to_origin.dot(dir_cross_e2)
      if (u < 0 || u > 1) { List() } else {
        val origin_cross_e1: RTTuple = p1_to_origin.cross(edges._1)
        val v: Double                = f * r.direction.dot(origin_cross_e1)
        if (v < 0 || (u + v) > 1) { List() } else {
          List(Intersection(f * edges._2.dot(origin_cross_e1), this))
        }
      }
    }
  }

  def localNormalAt(p: RTTuple, hit: Intersection): RTTuple = {
    normal
  }

  def bounds: (RTTuple, RTTuple) = {
    val xs: List[Double] = List(points._1, points._2, points._3).map((x: RTTuple) => x.x)
    val ys: List[Double] = List(points._1, points._2, points._3).map((x: RTTuple) => x.y)
    val zs: List[Double] = List(points._1, points._2, points._3).map((x: RTTuple) => x.z)
    (Point(xs.min, ys.min, zs.min), Point(xs.max, ys.max, zs.max))
  }
}

object Triangle {
  def apply(p1: RTTuple, p2: RTTuple, p3: RTTuple): Triangle = {
    val e1: RTTuple     = p2 - p1
    val e2: RTTuple     = p3 - p1
    val normal: RTTuple = e2.cross(e1).normalise()
    new Triangle(Matrix.getIdentityMatrix(4),
                 Material.defaultMaterial(),
                 true,
                 (p1, p2, p3),
                 (e1, e2),
                 normal)
  }
}
