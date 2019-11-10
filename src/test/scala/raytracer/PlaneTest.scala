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

class PlaneTest extends FunSuite {
  test("Plane.test_normal") {
    val p: Plane = Plane()
    val n1: RTTuple = p.localNormalAt(Point(0, 0, 0))
    val n2: RTTuple = p.localNormalAt(Point(10, 0, -10))
    val n3: RTTuple = p.localNormalAt(Point(-5, 0, 150))
    assert(n1 === Vector(0, 1, 0) && n2 === Vector(0, 1, 0) &&
      n3 === Vector(0, 1, 0))
  }

  test("Plane.test_parallel") {
    val p: Plane = Plane()
    val r: Ray = Ray(Point(0, 10, 0), Vector(0, 0, 1))

    val xs: Seq[Intersection] = p.localIntersect(r)
    assert(xs.isEmpty)
  }
  test("Plane.test_coplanar") {
    val p: Plane = Plane()
    val r: Ray = Ray(Point(0, 0, 0), Vector(0, 0, 1))

    val xs: Seq[Intersection] = p.localIntersect(r)
    assert(xs.isEmpty)
  }
  test("Plane.test_intersect_above") {
    val p: Plane = Plane()
    val r: Ray = Ray(Point(0, 1, 0), Vector(0, -1, 0))

    val xs: Seq[Intersection] = p.localIntersect(r)
    assert(xs.length === 1 && xs(0).t === 1 && xs(0).shape === p)
  }
  test("Plane.test_intersect_below") {
    val p: Plane = Plane()
    val r: Ray = Ray(Point(0, -1, 0), Vector(0, 1, 0))

    val xs: Seq[Intersection] = p.localIntersect(r)
    assert(xs.length === 1 && xs(0).t === 1 && xs(0).shape === p)
  }


}
