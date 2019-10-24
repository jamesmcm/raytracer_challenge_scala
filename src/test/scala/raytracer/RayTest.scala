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

class RayTest extends FunSuite {
  test("Ray.test_creation") {
    val origin: RTTuple = Point(1, 2, 3)
    val direction: RTTuple = Vector(4, 5, 6)
    val r: Ray = Ray(origin, direction)

    assert(r.origin === origin && r.direction === direction)
  }

  test("Ray.test_position") {
    val origin: RTTuple = Point(2, 3, 4)
    val direction: RTTuple = Vector(1, 0, 0)
    val r: Ray = Ray(origin, direction)

    assert(r.position(0) === Point(2, 3, 4) &&
      r.position(1) === Point(3, 3, 4) &&
      r.position(-1) === Point(1, 3, 4) &&
      r.position(2.5) === Point(4.5, 3, 4))
  }

  test("Ray.test_translate") {
    val r: Ray = Ray(Point(1,2,3), Vector(0,1,0))
    val m: Matrix = Translation(3,4,5)
    val r2: Ray = r.transform(m)

    assert(r2.origin === Point(4,6,8) && r2.direction === Vector(0,1,0))
  }
  test("Ray.test_scale") {
    val r: Ray = Ray(Point(1,2,3), Vector(0,1,0))
    val m: Matrix = Scaling(2,3,4)
    val r2: Ray = r.transform(m)

    assert(r2.origin === Point(2,6,12) && r2.direction === Vector(0,3,0))
  }


}
