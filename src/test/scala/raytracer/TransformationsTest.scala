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

  test("Matrix.test_halfquarter_xrotation") {
    assert(RotationX(math.Pi / 4).tupleMult(Point(0, 1, 0)) === Point(0, math.sqrt(2)/2, math.sqrt(2)/2))
  }
  test("Matrix.test_fullquarter_xrotation") {
    assert(RotationX(math.Pi / 2).tupleMult(Point(0, 1, 0)) === Point(0, 0, 1))
  }
  test("Matrix.test_halfquarter_inverse_xrotation") {
    assert(RotationX(math.Pi / 4).inverse.tupleMult(Point(0, 1, 0)) === Point(0, math.sqrt(2)/2, -math.sqrt(2)/2))
  }
  test("Matrix.test_halfquarter_yrotation") {
    assert(RotationY(math.Pi / 4).tupleMult(Point(0, 0, 1)) === Point(math.sqrt(2)/2, 0, math.sqrt(2)/2))
  }
  test("Matrix.test_fullquarter_yrotation") {
    assert(RotationY(math.Pi / 2).tupleMult(Point(0, 0, 1)) === Point(1, 0, 0))
  }
  test("Matrix.test_halfquarter_zrotation") {
    assert(RotationZ(math.Pi / 4).tupleMult(Point(0, 1, 0)) === Point(-math.sqrt(2)/2, math.sqrt(2)/2, 0))
  }
  test("Matrix.test_fullquarter_zrotation") {
    assert(RotationZ(math.Pi / 2).tupleMult(Point(0, 1, 0)) === Point(-1, 0, 0))
  }

  test("Matrix.test_shearing_x_y") {
    assert(Shearing(1, 0, 0, 0, 0, 0).tupleMult(Point(2, 3, 4)) === Point(5, 3, 4))
  }
  test("Matrix.test_shearing_x_z") {
    assert(Shearing(0, 1, 0, 0, 0, 0).tupleMult(Point(2, 3, 4)) === Point(6, 3, 4))
  }
  test("Matrix.test_shearing_y_x") {
    assert(Shearing(0, 0, 1, 0, 0, 0).tupleMult(Point(2, 3, 4)) === Point(2, 5, 4))
  }
  test("Matrix.test_shearing_y_z") {
    assert(Shearing(0, 0, 0, 1, 0, 0).tupleMult(Point(2, 3, 4)) === Point(2, 7, 4))
  }
  test("Matrix.test_shearing_z_x") {
    assert(Shearing(0, 0, 0, 0, 1, 0).tupleMult(Point(2, 3, 4)) === Point(2, 3, 6))
  }
  test("Matrix.test_shearing_z_y") {
    assert(Shearing(0, 0, 0, 0, 0, 1).tupleMult(Point(2, 3, 4)) === Point(2, 3, 7))
  }

  test("Matrix.test_transformations_sequence") {
    val p: RTTuple = Point(1, 0, 1)
    val p2: RTTuple = RotationX(Math.PI / 2).tupleMult(p)
    val p3: RTTuple = Scaling(5, 5, 5).tupleMult(p2)
    val p4: RTTuple = Translation(10, 5, 7).tupleMult(p3)

    assert(p2 === Point(1, -1, 0) && p3 === Point(5, -5, 0) && p4 === Point(15, 0, 7))
  }
  test("Matrix.test_transformations_chained") {
    val p: RTTuple = Point(1, 0, 1)
    val A: Matrix = RotationX(Math.PI / 2)
    val B: Matrix = Scaling(5, 5, 5)
    val C: Matrix = Translation(10, 5, 7)

    assert((C*B*A).tupleMult(p) === Point(15, 0, 7))
  }

  test("Matrix.test_transformations_chained_matrix") {
    val s1: String = """| 1 | 1 | 1 | 1 |
                        | 0 | 0 | 0 | 0 |
                        | 1 | 1 | 1 | 1 |
                        | 1 | 1 | 1 | 1 |""".stripMargin
    val res: String = """| 15 | 15 | 15 | 15 |
                        | 0 | 0 | 0 | 0 |
                        | 7 | 7 | 7 | 7 |
                        | 1 | 1 | 1 | 1 |""".stripMargin
    val m1: Matrix = Matrix.matrixFromString(s1)
    val mres: Matrix = Matrix.matrixFromString(res)

    val m2: Matrix = m1.rotateX(math.Pi/2).scale(5,5,5).translate(10, 5, 7)

    assert(m2 === mres)
  }

  test("Matrix.view_transformation_identity") {
    assert(viewTransform(Point(0,0,0), Point(0,0,-1), Vector(0,1,0)) === Matrix.getIdentityMatrix(4))
  }
  test("Matrix.view_transformation_back") {
    assert(viewTransform(Point(0,0,0), Point(0,0,1), Vector(0,1,0)) === Scaling(-1, 1, -1))
  }
  test("Matrix.view_transformation_world") {
    assert(viewTransform(Point(0,0,8), Point(0,0,0), Vector(0,1,0)) === Translation(0, 0, -8))
  }
  test("Matrix.view_transformation_arbitrary") {
    val res: String = """| -0.50709 | 0.50709 |  0.67612 | -2.36643 |
                         |  0.76772 | 0.60609 |  0.12122 | -2.82843 |
                         | -0.35857 | 0.59761 | -0.71714 |  0.00000 |
                         |  0.00000 | 0.00000 |  0.00000 |  1.00000 |""".stripMargin
    val mres: Matrix = Matrix.matrixFromString(res)
    assert(viewTransform(Point(1,3,2), Point(4,-2,8), Vector(1,1,0)) === mres)
  }
}
