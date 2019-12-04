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

class CylinderTest extends FunSuite {
  test("Cylinder.test_miss1") {
    val c: Cylinder    = Cylinder()
    val r: Ray = Ray(Point(1, 0, 0), Vector(0,1,0).normalise())

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.isEmpty)
  }
  test("Cylinder.test_miss2") {
    val c: Cylinder    = Cylinder()
    val r: Ray = Ray(Point(0, 0, 0), Vector(0,1,0).normalise())

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.isEmpty)
  }
  test("Cylinder.test_miss3") {
    val c: Cylinder    = Cylinder()
    val r: Ray = Ray(Point(0, 0, -5), Vector(1,1,1).normalise())

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.isEmpty)
  }
  test("Cylinder.test_intersect1") {
    val c: Cylinder    = Cylinder()
    val r: Ray = Ray(Point(1, 0, -5), Vector(0,0,1).normalise())

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 2 && xs(0).t === 5 && xs(1).t === 5)
  }
  test("Cylinder.test_intersect2") {
    val c: Cylinder    = Cylinder()
    val r: Ray = Ray(Point(0, 0, -5), Vector(0,0,1).normalise())

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 2 && xs(0).t === 4 && xs(1).t === 6)
  }
  test("Cylinder.test_intersect3") {
    val c: Cylinder    = Cylinder()
    val r: Ray = Ray(Point(0.5, 0, -5), Vector(0.1,1,1).normalise())

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 2 && doubleEq(xs(0).t, 6.80798) && doubleEq(xs(1).t, 7.08872))
  }
  test("Cylinder.test_normal1") {
    val c: Cylinder    = Cylinder()
    val n: RTTuple = c.localNormalAt(Point(1,0,0))

    assert(n === Vector(1,0,0))
  }
  test("Cylinder.test_normal2") {
    val c: Cylinder    = Cylinder()
    val n: RTTuple = c.localNormalAt(Point(0,5,-1))

    assert(n === Vector(0,0,-1))
  }
  test("Cylinder.test_normal3") {
    val c: Cylinder    = Cylinder()
    val n: RTTuple = c.localNormalAt(Point(0,-2,1))

    assert(n === Vector(0,0,1))
  }
  test("Cylinder.test_normal4") {
    val c: Cylinder    = Cylinder()
    val n: RTTuple = c.localNormalAt(Point(-1,1,0))

    assert(n === Vector(-1,0,0))
  }
}
