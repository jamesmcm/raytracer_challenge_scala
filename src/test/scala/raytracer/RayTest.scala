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

}
