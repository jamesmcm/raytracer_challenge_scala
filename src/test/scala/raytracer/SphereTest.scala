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

class SphereTest extends FunSuite {
  test("Sphere.test_intersect1") {
    val r: Ray = Ray(Point(0,0,-5), Vector(0,0,1))
    val s: Sphere = Sphere.unitSphere()

    val xs: Seq[Intersection] = s.intersect(r)
    assert(xs.length === 2 && xs(0).t === 4.0 && xs(1).t === 6.0)
  }
  test("Sphere.test_intersect_tangent") {
    val r: Ray = Ray(Point(0,1,-5), Vector(0,0,1))
    val s: Sphere = Sphere.unitSphere()

    val xs: Seq[Intersection] = s.intersect(r)
    assert(xs.length === 2 && xs(0).t === 5.0 && xs(1).t === 5.0)
  }
  test("Sphere.test_intersect_miss") {
    val r: Ray = Ray(Point(0,2,-5), Vector(0,0,1))
    val s: Sphere = Sphere.unitSphere()

    val xs: Seq[Intersection] = s.intersect(r)
    assert(xs.length === 0)
  }
  test("Sphere.test_intersect_origin_inside") {
    val r: Ray = Ray(Point(0,0,0), Vector(0,0,1))
    val s: Sphere = Sphere.unitSphere()

    val xs: Seq[Intersection] = s.intersect(r)
    assert(xs.length === 2 && xs(0).t === -1.0 && xs(1).t === 1.0)
  }
  test("Sphere.test_intersect_sphere_behind") {
    val r: Ray = Ray(Point(0,0,5), Vector(0,0,1))
    val s: Sphere = Sphere.unitSphere()

    val xs: Seq[Intersection] = s.intersect(r)
    assert(xs.length === 2 && xs(0).t === -6.0 && xs(1).t === -4.0)
  }

  test("Sphere.test_intersect_object_set") {
    val r: Ray = Ray(Point(0,0,-5), Vector(0,0,1))
    val s: Sphere = Sphere.unitSphere()

    val xs: Seq[Intersection] = s.intersect(r)
    assert(xs.length === 2 && xs(0).shape === s && xs(1).shape === s)
  }

  test("Sphere.test_intersect_scaling") {
    val r: Ray = Ray(Point(0,0,-5), Vector(0,0,1))
    val s: Sphere = Sphere.unitSphere().setTransform(Scaling(2,2,2))

    val xs: Seq[Intersection] = s.intersect(r)
    assert(xs.length === 2 && xs(0).t === 3 && xs(1).t === 7)
  }

}
