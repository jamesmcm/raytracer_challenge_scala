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

class TriangleTest extends FunSuite {
  test("Triangle.test_construction") {
    val t: Triangle = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    assert(
      t.points === (Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0)) &&
        t.edges._1 === Vector(-1, -1, 0) &&
        t.edges._2 === Vector(1, -1, 0) &&
        t.normal === Vector(0, 0, -1))
  }

  test("Triangle.test_normal") {
    val t: Triangle = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    assert(t.localNormalAt(Point(0,0.5,0)) === t.normal &&
      t.localNormalAt(Point(-0.5,0.75,0)) === t.normal &&
      t.localNormalAt(Point(0.5,0.25,0)) === t.normal)
  }

  test("Triangle.intersect_parallel") {
    val t: Triangle = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r: Ray = Ray(Point(0, -1, -2), Vector(0, 1, 0))
    assert(t.localIntersect(r).isEmpty)
  }

  test("Triangle.intersect_edge_miss_p1p3") {
    val t: Triangle = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r: Ray = Ray(Point(1, 1, -2), Vector(0, 0, 1))
    assert(t.localIntersect(r).isEmpty)
  }
  test("Triangle.intersect_edge_miss_p1p2") {
    val t: Triangle = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r: Ray = Ray(Point(-1, 1, -2), Vector(0, 0, 1))
    assert(t.localIntersect(r).isEmpty)
  }
  test("Triangle.intersect_edge_miss_p2p3") {
    val t: Triangle = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r: Ray = Ray(Point(0, -1, -2), Vector(0, 0, 1))
    assert(t.localIntersect(r).isEmpty)
  }
  test("Triangle.intersect_hit") {
    val t: Triangle = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r: Ray = Ray(Point(0, 0.5, -2), Vector(0, 0, 1))
    val xs: Seq[Intersection] = t.localIntersect(r)
    assert(xs.length === 1 && doubleEq(xs(0).t, 2))
  }
}
