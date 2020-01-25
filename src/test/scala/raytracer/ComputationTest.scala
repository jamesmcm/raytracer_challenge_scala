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

class ComputationTest extends FunSuite {
  test("Computation.test_creation") {
    val r: Ray          = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val s: Sphere       = Sphere.unitSphere()
    val i: Intersection = Intersection(4, s)

    val comps: Computation = Computation.prepareComputations(i, r, Intersection.intersections(i))
    assert(
      comps.t === i.t && comps.shape === i.shape && comps.point === Point(0, 0, -1)
        && comps.eyev === Vector(0, 0, -1) && comps.normalv === Vector(0, 0, -1))
  }
  test("Computation.test_outside_hit") {
    val r: Ray          = Ray(Point(0, 0, -5), Vector(0, 0, 1))
    val s: Sphere       = Sphere.unitSphere()
    val i: Intersection = Intersection(4, s)

    val comps: Computation = Computation.prepareComputations(i, r, Intersection.intersections(i))
    assert(!comps.inside)
  }
  test("Computation.test_inside_hit") {
    val r: Ray          = Ray(Point(0, 0, 0), Vector(0, 0, 1))
    val s: Sphere       = Sphere.unitSphere()
    val i: Intersection = Intersection(1, s)

    val comps: Computation = Computation.prepareComputations(i, r, Intersection.intersections(i))
    assert(
      comps.point === Point(0, 0, 1) && comps.eyev === Vector(0, 0, -1)
        && comps.inside && comps.normalv === Vector(0, 0, -1))
  }
}
