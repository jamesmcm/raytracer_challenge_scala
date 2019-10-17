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

class MatrixTest extends FunSuite {
  test("Matrix.test_construction1") {
    val s: String =
      """| 1 | 2 | 3 | 4 |
         | 5.5 | 6.5 | 7.5 | 8.5 |
         | 9 | 10 | 11 | 12 |
         | 13.5 | 14.5 | 15.5 | 16.5 |""".stripMargin

    val m: Matrix = Matrix.matrixFromString(s)
    assert(m.m(0)(0) === 1 && m.m(0)(3) === 4 && m.m(1)(0) === 5.5 && m.m(1)(2) === 7.5 &&
      m.m(2)(2) === 11 && m.m(3)(0) === 13.5 && m.m(3)(2) === 15.5)
  }
  test("Matrix.test_construction2") {
    val s: String =
      """| -3 | 5 |
         | 1 | -2 |""".stripMargin

    val m: Matrix = Matrix.matrixFromString(s)
    assert(m(0,0) === -3 && m(0,1) === 5 && m(1,0) === 1 && m(1,1) === -2)
  }
  test("Matrix.test_construction3") {
    val s: String =
      """|-3|5|0|
         |1|-2|-7|
         |0|1|1|""".stripMargin

    val m: Matrix = Matrix.matrixFromString(s)
    assert(m(0,0) === -3 && m(1,1) === -2 && m(2,2) === 1)
  }
  test("Matrix.test_equality1") {
    val s1: String = """| 1 | 2 | 3 | 4 |
| 5 | 6 | 7 | 8 |
| 9 | 8 | 7 | 6 |
| 5 | 4 | 3 | 2 |""".stripMargin

    val s2: String = """| 1 | 2 | 3 | 4 |
| 5 | 6 | 7 | 8 |
| 9 | 8 | 7 | 6 |
| 5 | 4 | 3 | 2 |""".stripMargin

    val m1: Matrix = Matrix.matrixFromString(s1)
    val m2: Matrix = Matrix.matrixFromString(s2)
    assert(m1 === m2)
  }
  test("Matrix.test_equality2") {
    val s1: String = """| 1 | 2 | 3 | 4 |
                        | 5 | 6 | 7 | 8 |
                        | 9 | 8 | 7 | 6 |
                        | 5 | 4 | 3 | 2 |""".stripMargin

    val s2: String = """| 2 | 3 | 4 | 5 |
| 6 | 7 | 8 | 9 |
| 8 | 7 | 6 | 5 |
| 4 | 3 | 2 | 1 |""".stripMargin

    val m1: Matrix = Matrix.matrixFromString(s1)
    val m2: Matrix = Matrix.matrixFromString(s2)
    assert(!(m1 === m2))
  }
  test("Matrix.test_multiply1") {
    val s1: String = """| 1 | 2 | 3 | 4 |
                        | 5 | 6 | 7 | 8 |
                        | 9 | 8 | 7 | 6 |
                        | 5 | 4 | 3 | 2 |""".stripMargin

    val s2: String = """| -2 | 1 | 2 | 3 |
                        | 3 | 2 | 1 | -1 |
                        | 4 | 3 | 6 | 5 |
                        | 1 | 2 | 7 | 8 |""".stripMargin

    val res: String = """| 20 | 22 | 50 | 48 |
                        | 44 | 54 | 114 | 108 |
                        | 40 | 58 | 110 | 102 |
                        | 16 | 26 | 46 | 42 |""".stripMargin

    val m1: Matrix = Matrix.matrixFromString(s1)
    val m2: Matrix = Matrix.matrixFromString(s2)
    val mres: Matrix = Matrix.matrixFromString(res)

    assert((m1*m2) === mres)
  }
  test("Matrix.test_multiply2") {
    val s1: String = """| 1 | 2 | 3 | 4 |
                        | 2 | 4 | 4 | 2 |
                        | 8 | 6 | 4 | 1 |
                        | 0 | 0 | 0 | 1 |""".stripMargin

    val m1: Matrix = Matrix.matrixFromString(s1)

    val tuple1: RTTuple = Point(1, 2, 3)
    val res: RTTuple = Point(18, 24, 33)

    assert((m1 tupleMult tuple1) === res)
  }
  test("Matrix.test_identitymatrix1") {
    val s1: String = """| 1 | 0 | 0 | 0 |
                        | 0 | 1 | 0 | 0 |
                        | 0 | 0 | 1 | 0 |
                        | 0 | 0 | 0 | 1 |""".stripMargin

    val m1: Matrix = Matrix.matrixFromString(s1)

    assert(m1 === Matrix.getIdentityMatrix(4))
  }
  test("Matrix.test_identitymatrix2") {
    val s1: String = """| 0 | 1 | 2 | 4 |
                        | 1 | 2 | 4 | 8 |
                        | 2 | 4 | 8 | 16 |
                        | 4 | 8 | 16 | 32 |""".stripMargin

    val m1: Matrix = Matrix.matrixFromString(s1)

    assert(m1 * Matrix.getIdentityMatrix(4) === m1)
  }

  test("Matrix.test_transpose1") {
    val s1: String = """| 0 | 9 | 3 | 0 |
                        | 9 | 8 | 0 | 8 |
                        | 1 | 8 | 5 | 3 |
                        | 0 | 0 | 5 | 8 |""".stripMargin
    val res: String = """| 0 | 9 | 1 | 0 |
                        | 9 | 8 | 8 | 0 |
                        | 3 | 0 | 5 | 5 |
                        | 0 | 8 | 3 | 8 |""".stripMargin

    val m1: Matrix = Matrix.matrixFromString(s1)
    val mres: Matrix = Matrix.matrixFromString(res)

    assert(m1.transpose === mres)
  }
  test("Matrix.test_transpose2") {
    assert(Matrix.getIdentityMatrix(4).transpose === Matrix.getIdentityMatrix(4))
  }
  }
