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

class Cylinder(val transform: Matrix,
               val material: Material,
               val minimum: Double,
               val maximum: Double,
               val closed: Boolean,
               val shadow: Boolean)
    extends SpaceObject {
  type T = Cylinder

  def constructor(t: Matrix, m: Material, s: Boolean): T =
    new Cylinder(t, m, Double.NegativeInfinity, Double.PositiveInfinity, false, s)

  final override def equals(that: Any): Boolean = {
    that match {
      case that: Cylinder => transform === that.transform && material === that.material
      case _              => false
    }
  }

  final override def hashCode: Int = (transform, material).##

  def setMinimum(x: Double): Cylinder = {
    new Cylinder(transform, material, x, maximum, closed, shadow)
  }
  def setMaximum(x: Double): Cylinder = {
    new Cylinder(transform, material, minimum, x, closed, shadow)
  }
  def setClosed(x: Boolean): Cylinder = {
    new Cylinder(transform, material, minimum, maximum, x, shadow)
  }

  def intersectCaps(r: Ray): Seq[Intersection] = {
    if ((!closed) || doubleEq(r.direction.y, 0) ) List() else {

      ((minimum - r.origin.y)/r.direction.y, (maximum - r.origin.y)/r.direction.y) match {
        case (a, b) if Cylinder.checkCap(r, a) && Cylinder.checkCap(r, b) => List(Intersection(a, this), Intersection(b, this))
        case (a, b) if Cylinder.checkCap(r, a)  => List(Intersection(a, this))
        case (a, b) if Cylinder.checkCap(r, b)  => List(Intersection(b, this))
        case _ => List()
      }
    }
  }

  def localIntersect(r: Ray): Seq[Intersection] = {
    val a: Double = r.direction.x * r.direction.x + r.direction.z * r.direction.z
    if (doubleEq(a, 0)) (intersectCaps(r))
    else {

      val b: Double    = 2 * r.origin.x * r.direction.x + 2 * r.origin.z * r.direction.z
      val c: Double    = r.origin.x * r.origin.x + r.origin.z * r.origin.z - 1
      val disc: Double = (b * b) - 4 * a * c
      if (disc < 0) intersectCaps(r)
      else {
        val t0: Double = (-b - math.sqrt(disc)) / (2 * a)
        val t1: Double = (-b + math.sqrt(disc)) / (2 * a)

        //TODO: Ensure left is smaller
        (r.origin.y + t0 * r.direction.y, r.origin.y + t1 * r.direction.y) match {
          case (a, b) if (a > minimum && a < maximum && b > minimum && b < maximum) =>
            List(Intersection(t0, this), Intersection(t1, this))
          case (a, b) if (a > minimum && a < maximum) => List(Intersection(t0, this)) ++ intersectCaps(r)
          case (a, b) if (b > minimum && b < maximum) =>
            List(Intersection(t1, this)) ++ intersectCaps(r)
          case _ => intersectCaps(r)
        }
      }
    }
  }

  def localNormalAt(p: RTTuple): RTTuple = {
    val dist: Double = p.x*p.x + p.z*p.z

    if (dist<1 && p.y >= maximum - EPSILON) Vector(0, 1, 0) else {
      if (dist<1 && p.y <= minimum + EPSILON) Vector(0, -1, 0) else {
        Vector(p.x, 0, p.z)
      }
    }
  }
  def bounds: (RTTuple, RTTuple) = {
    (Point(-1, minimum, -1), Point(1, maximum, 1))
  }
}

object Cylinder {
  def apply(): Cylinder =
    new Cylinder(Matrix.getIdentityMatrix(4),
                 Material.defaultMaterial(),
                 Double.NegativeInfinity,
                 Double.PositiveInfinity,
                 false, shadow=true)

  def checkCap(r: Ray, t: Double): Boolean = {
    val x: Double = (r.origin.x + r.direction.x * t)
    val z: Double = (r.origin.z + r.direction.z * t)

    (x * x + z * z) <= 1
  }
}
