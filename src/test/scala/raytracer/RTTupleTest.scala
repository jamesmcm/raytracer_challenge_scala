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
  test("RTTuple.test_vector_sub_zero") {
    assert((Vector(0, 0, 0) - Vector(1, -2, 3)) === Vector(-1, 2, -3))
  }
  test("RTTuple.test_tuple_negate") {
    val t = new RTTuple(1, -2, 3 ,-4)
    assert(t.negate() === new RTTuple(-1, 2, -3, 4))
  }
  test("RTTuple.test_tuple_multiply1") {
    val t = new RTTuple(1, -2, 3 ,-4)
    assert(t*3.5 === new RTTuple(3.5, -7, 10.5, -14))
  }
  test("RTTuple.test_tuple_multiply2") {
    val t = new RTTuple(1, -2, 3 ,-4)
    assert(t*0.5 === new RTTuple(0.5, -1, 1.5, -2))
  }
  test("RTTuple.test_tuple_divide1") {
    val t = new RTTuple(1, -2, 3 ,-4)
    assert(t/2 === new RTTuple(0.5, -1, 1.5, -2))
  }
  test("RTTuple.test_vector_magnitude1") {
    assert(Vector(1, 0, 0).magnitude() === 1)
  }
  test("RTTuple.test_vector_magnitude2") {
    assert(Vector(0, 1, 0).magnitude() === 1)
  }
  test("RTTuple.test_vector_magnitude3") {
    assert(Vector(0, 0, 1).magnitude() === 1)
  }
  test("RTTuple.test_vector_magnitude4") {
    assert(Vector(1, 2, 3).magnitude() === math.sqrt(14))
  }
  test("RTTuple.test_vector_magnitude5") {
    assert(Vector(-1, -2, -3).magnitude() === math.sqrt(14))
  }
  test("RTTuple.test_vector_normalise1") {
    assert(Vector(4, 0, 0).normalise() === Vector(1, 0, 0))
  }
  test("RTTuple.test_vector_normalise2") {
    assert(Vector(1, 2, 3).normalise() === Vector(0.26726, 0.53452, 0.80178))
  }
  test("RTTuple.test_vector_normalise_magnitude") {
    assert(Vector(1, 2, 3).normalise().magnitude() === 1 )
  }
  test("RTTuple.test_vector_dot") {
    assert((Vector(1, 2, 3) dot Vector(2,3,4)) === 20)
  }
  test("RTTuple.test_vector_cross1") {
    assert((Vector(1, 2, 3) cross Vector(2,3,4)) === Vector(-1, 2, -1))
  }
  test("RTTuple.test_vector_cross2") {
    assert((Vector(2,3,4) cross Vector(1, 2, 3) ) === Vector(1, -2, 1))
  }
  test("RTTuple.test_projectile1") {
    val p = Projectile(Point(0, -1, 0), Vector(1, 0, 0).normalise())
    val e = new ParticleEnvironment(Vector(0, -0.1, 0), Vector(-0.01, 0, 0))
    assert(e.ticksToLand(p, 0) === 0)
  }
  test("RTTuple.test_projectile2") {
    val p = Projectile(Point(0, 1, 0), Vector(1, 0, 0).normalise())
    val e = new ParticleEnvironment(Vector(0, -0.1, 0), Vector(-0.01, 0, 0))
    assert(e.ticksToLand(p, 0) === 5)
  }
  test("RTTuple.test_reflect1") {
    assert(Vector(1, -1, 0).reflect(Vector(0, 1, 0)) === Vector(1, 1, 0))
  }
  test("RTTuple.test_reflect2") {
    assert(Vector(0, -1, 0).reflect(Vector(math.sqrt(2)/2, math.sqrt(2)/2, 0)) === Vector(1, 0, 0))
  }
}
