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

class RTTupleTest extends FunSuite {
  test("RTTuple.test_point_base") {
    val t = new RTTuple(4.3, -4.2, 3.1, 1)
    assert(t.x === 4.3 && t.y === -4.2 && t.z === 3.1 && t.w === 1)
  }

  test("RTTuple.test_vector_base") {
    val t = new RTTuple(4.3, -4.2, 3.1, 0)
    assert(t.x === 4.3 && t.y === -4.2 && t.z === 3.1 && t.w === 0)
  }

  test("RTTuple.test_point_tuple") {
    assert(Point(4, -4, 3).toTuple === Tuple4[Double, Double, Double, Int](4, -4, 3, 1))
  }
  test("RTTuple.test_vector_tuple") {
    assert(Vector(4, -4, 3).toTuple === Tuple4[Double, Double, Double, Int](4, -4, 3, 0))
  }

  test("RTTuple.test_vector_eq") {
    assert(Vector(4, -4, 3) === Vector(4, -4, 3) )
  }
  test("RTTuple.test_point_eq") {
    assert(Point(4, -4, 3) === Point(3.999999999999, -4, 3) )
  }

  test("RTTuple.test_point_vec_add") {
    assert((Vector(-2, 3, 1) + Point(3, -2, 5)).toTuple === Tuple4[Double, Double, Double, Int](1, 1, 6, 1))
  }
  test("RTTuple.test_point_point_sub") {
    assert((Point(3, 2, 1) - Point(5, 6, 7)).toTuple === Tuple4[Double, Double, Double, Int](-2, -4, -6, 0))
  }
  test("RTTuple.test_point_vector_sub") {
    assert((Point(3, 2, 1) - Vector(5, 6, 7)).toTuple === Tuple4[Double, Double, Double, Int](-2, -4, -6, 1))
  }
  test("RTTuple.test_vector_vector_sub") {
    assert((Vector(3, 2, 1) - Vector(5, 6, 7)) === Vector(-2, -4, -6))
  }
}
