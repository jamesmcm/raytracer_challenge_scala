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

class ConeTest extends FunSuite {
  test("Cone.test_intersect1") {
    val c: Cone = Cone()
    val r: Ray  = Ray(Point(0, 0, -5), Vector(0, 0, 1).normalise())

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 2 && doubleEq(xs(0).t, 5) && doubleEq(xs(1).t, 5))
  }
  test("Cone.test_intersect2") {
    val c: Cone = Cone()
    val r: Ray  = Ray(Point(0, 0, -5), Vector(1, 1, 1).normalise())

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 2 && doubleEq(xs(0).t, 8.66025) && doubleEq(xs(1).t, 8.66025))
  }
  test("Cone.test_intersect3") {
    val c: Cone = Cone()
    val r: Ray  = Ray(Point(1, 1, -5), Vector(-0.5, -1, 1).normalise())

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 2 && doubleEq(xs(0).t, 4.55006) && doubleEq(xs(1).t, 49.44994))
  }
  test("Cone.test_intersect_parallel") {
    val c: Cone = Cone()
    val r: Ray  = Ray(Point(0, 0, -1), Vector(0, 1, 1).normalise())

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 1 && doubleEq(xs(0).t, 0.35355))
  }
  test("Cone.test_intersect_caps1") {
    val c: Cone = Cone().setMinimum(-0.5).setMaximum(0.5).setClosed(true)
    val r: Ray  = Ray(Point(0, 0, -5), Vector(0, 1, 0).normalise())

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.isEmpty)
  }
  test("Cone.test_intersect_caps2") {
    val c: Cone = Cone().setMinimum(-0.5).setMaximum(0.5).setClosed(true)
    val r: Ray  = Ray(Point(0, 0, -0.25), Vector(0, 1, 1).normalise())

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 2)
  }
  test("Cone.test_intersect_caps3") {
    val c: Cone = Cone().setMinimum(-0.5).setMaximum(0.5).setClosed(true)
    val r: Ray  = Ray(Point(0, 0, -0.25), Vector(0, 1, 0).normalise())

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 4)
  }
  test("Cone.test_normal1") {
    val c: Cone    = Cone()
    val n: RTTuple = c.localNormalAt(Point(0, 0, 0), Intersection(0, c))

    assert(n === Vector(0, 0, 0))
  }
  test("Cone.test_normal2") {
    val c: Cone    = Cone()
    val n: RTTuple = c.localNormalAt(Point(1, 1, 1), Intersection(0, c))

    assert(n === Vector(1, -math.sqrt(2), 1))
  }
  test("Cone.test_normal3") {
    val c: Cone    = Cone()
    val n: RTTuple = c.localNormalAt(Point(-1, -1, 0), Intersection(0, c))

    assert(n === Vector(-1, 1, 0))
  }
}
