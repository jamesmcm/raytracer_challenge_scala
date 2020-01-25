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

class CubeTest extends FunSuite {
  test("Cube.test_intersect1") {
    val c: Cube = Cube()
    val r: Ray  = Ray(Point(5, 0.5, 0), Vector(-1, 0, 0))

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 2 && xs(0).t === 4 && xs(1).t === 6)
  }
  test("Cube.test_intersect2") {
    val c: Cube = Cube()
    val r: Ray  = Ray(Point(-5, 0.5, 0), Vector(1, 0, 0))

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 2 && xs(0).t === 4 && xs(1).t === 6)
  }
  test("Cube.test_intersect3") {
    val c: Cube = Cube()
    val r: Ray  = Ray(Point(0.5, 5, 0), Vector(0, -1, 0))

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 2 && xs(0).t === 4 && xs(1).t === 6)
  }
  test("Cube.test_intersect4") {
    val c: Cube = Cube()
    val r: Ray  = Ray(Point(0.5, -5, 0), Vector(0, 1, 0))

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 2 && xs(0).t === 4 && xs(1).t === 6)
  }
  test("Cube.test_intersect5") {
    val c: Cube = Cube()
    val r: Ray  = Ray(Point(0.5, 0, 5), Vector(0, 0, -1))

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 2 && xs(0).t === 4 && xs(1).t === 6)
  }
  test("Cube.test_intersect6") {
    val c: Cube = Cube()
    val r: Ray  = Ray(Point(0.5, 0, -5), Vector(0, 0, 1))

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 2 && xs(0).t === 4 && xs(1).t === 6)
  }
  test("Cube.test_intersect7") {
    val c: Cube = Cube()
    val r: Ray  = Ray(Point(0, 0.5, 0), Vector(0, 0, 1))

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.length === 2 && xs(0).t === -1 && xs(1).t === 1)
  }
  test("Cube.test_miss1") {
    val c: Cube = Cube()
    val r: Ray  = Ray(Point(-2, 0, 0), Vector(0.2673, 0.5345, 0.8018))

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.isEmpty)
  }
  test("Cube.test_miss2") {
    val c: Cube = Cube()
    val r: Ray  = Ray(Point(0, -2, 0), Vector(0.8018, 0.2673, 0.5345))

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.isEmpty)
  }
  test("Cube.test_miss3") {
    val c: Cube = Cube()
    val r: Ray  = Ray(Point(0, 0, -2), Vector(0.5345, 0.8018, 0.2673))

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.isEmpty)
  }
  test("Cube.test_miss4") {
    val c: Cube = Cube()
    val r: Ray  = Ray(Point(2, 0, 2), Vector(0, 0, -1))

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.isEmpty)
  }
  test("Cube.test_miss5") {
    val c: Cube = Cube()
    val r: Ray  = Ray(Point(0, 2, 2), Vector(0, -1, 0))

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.isEmpty)
  }
  test("Cube.test_miss6") {
    val c: Cube = Cube()
    val r: Ray  = Ray(Point(2, 2, 0), Vector(-1, 0, 0))

    val xs: Seq[Intersection] = c.localIntersect(r)
    assert(xs.isEmpty)
  }
  test("Cube.test_normal1") {
    val c: Cube    = Cube()
    val p: RTTuple = Point(1, 0.5, -0.8)

    assert(c.localNormalAt(p, Intersection(0, c)) === Vector(1, 0, 0))
  }
  test("Cube.test_normal2") {
    val c: Cube    = Cube()
    val p: RTTuple = Point(-1, -0.2, 0.9)

    assert(c.localNormalAt(p, Intersection(0, c)) === Vector(-1, 0, 0))
  }
  test("Cube.test_normal3") {
    val c: Cube    = Cube()
    val p: RTTuple = Point(-0.4, 1, -0.1)

    assert(c.localNormalAt(p, Intersection(0, c)) === Vector(0, 1, 0))
  }
  test("Cube.test_normal4") {
    val c: Cube    = Cube()
    val p: RTTuple = Point(0.3, -1, -0.7)

    assert(c.localNormalAt(p, Intersection(0, c)) === Vector(0, -1, 0))
  }
  test("Cube.test_normal5") {
    val c: Cube    = Cube()
    val p: RTTuple = Point(-0.6, 0.3, 1)

    assert(c.localNormalAt(p, Intersection(0, c)) === Vector(0, 0, 1))
  }
  test("Cube.test_normal6") {
    val c: Cube    = Cube()
    val p: RTTuple = Point(0.4, 0.4, -1)

    assert(c.localNormalAt(p, Intersection(0, c)) === Vector(0, 0, -1))
  }
  test("Cube.test_normal7") {
    val c: Cube    = Cube()
    val p: RTTuple = Point(1, 1, 1)

    assert(c.localNormalAt(p, Intersection(0, c)) === Vector(1, 0, 0))
  }
  test("Cube.test_normal8") {
    val c: Cube    = Cube()
    val p: RTTuple = Point(-1, -1, -1)

    assert(c.localNormalAt(p, Intersection(0, c)) === Vector(-1, 0, 0))
  }

}
