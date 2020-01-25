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

import org.scalatest.FunSuite

class SmoothTriangleTest extends FunSuite {
  test("SmoothTriangle.test_construction") {
    val t: SmoothTriangle = SmoothTriangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0),
      Vector(0, 1, 0), Vector(-1, 0, 0), Vector(1, 0 ,0))
    assert(
      t.points === (Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0)) &&
      t.normals === (Vector(0, 1, 0), Vector(-1, 0, 0), Vector(1, 0 ,0)))
  }
  test("SmoothTriangle.test_construction_intersection") {
    val t: Triangle = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val i: Intersection = Intersection.intersectionWithUV(3.5, t, 0.2, 0.4)
    assert(i.u === 0.2 && i.v === 0.4)
  }
  test("SmoothTriangle.test_intersection_smooth") {
    val t: SmoothTriangle = SmoothTriangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0),
      Vector(0, 1, 0), Vector(-1, 0, 0), Vector(1, 0 ,0))
    val r: Ray = Ray(Point(-0.2, 0.3, -2), Vector(0, 0, 1))
    val xs: Seq[Intersection] = t.localIntersect(r)
    assert(doubleEq(xs(0).u, 0.45) && doubleEq(xs(0).v, 0.25))
  }
  test("SmoothTriangle.test_normal") {
    val t: SmoothTriangle = SmoothTriangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0),
      Vector(0, 1, 0), Vector(-1, 0, 0), Vector(1, 0 ,0))
    val i: Intersection = Intersection.intersectionWithUV(1, t, 0.45, 0.25)
    val n: RTTuple = t.normalAt(Point(0,0,0), i)
    assert(n === Vector(-0.5547, 0.83205, 0))
  }
  test("SmoothTriangle.test_comps") {
    val t: SmoothTriangle = SmoothTriangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0),
      Vector(0, 1, 0), Vector(-1, 0, 0), Vector(1, 0 ,0))
    val i: Intersection = Intersection.intersectionWithUV(1, t, 0.45, 0.25)
    val r: Ray = Ray(Point(-0.2, 0.3, -2), Vector(0, 0, 1))
    val xs: Seq[Intersection] = Intersection.intersections(i)
    val comps: Computation = Computation.prepareComputations(i, r, xs)

    assert(comps.normalv === Vector(-0.5547, 0.83205, 0))
  }
}
