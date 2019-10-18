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

class TransformationsTest extends FunSuite {

  test("Matrix.test_translation_point") {
    assert(Translation(5, -3, 2).tupleMult(Point(-3, 4, 5)) === Point(2, 1, 7))
  }
  test("Matrix.test_translation_inverse") {
    assert(Translation(5, -3, 2).inverse.tupleMult(Point(-3, 4, 5)) === Point(-8, 7, 3))
  }
  test("Matrix.test_translation_vector") {
    assert(Translation(5, -3, 2).tupleMult(Vector(-3, 4, 5)) === Vector(-3, 4, 5))
  }

  test("Matrix.test_scaling_point") {
    assert(Scaling(2, 3, 4).tupleMult(Point(-4, 6, 8)) === Point(-8, 18, 32))
  }
  test("Matrix.test_scaling_vector") {
    assert(Scaling(2, 3, 4).tupleMult(Vector(-4, 6, 8)) === Vector(-8, 18, 32))
  }
  test("Matrix.test_scaling_inverse") {
    assert(Scaling(2, 3, 4).inverse.tupleMult(Vector(-4, 6, 8)) === Vector(-2, 2, 2))
  }
  test("Matrix.test_scaling_reflection") {
    assert(Scaling(-1, 1, 1).tupleMult(Point(2, 3, 4)) === Point(-2, 3, 4))
  }

}
