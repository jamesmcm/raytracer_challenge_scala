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

  test("Matrix.test_determinant2d") {
    val s1: String = """| 1 | 5 |
                        | -3 | 2 |""".stripMargin

    val m1: Matrix = Matrix.matrixFromString(s1)

    assert(doubleEq(m1.determinant, 17))
  }
  test("Matrix.test_submatrix1") {
    val s1: String = """| 1 | 5 | 0 |
                        | -3 | 2 | 7 |
                        | 0 | 6 | 3 |""".stripMargin
    val res: String = """| -3 | 2 |
                        | 0 | 6 |""".stripMargin

    val m1: Matrix = Matrix.matrixFromString(s1)
    val mres: Matrix = Matrix.matrixFromString(res)

    assert(m1.submatrix(0, 2) === mres)
  }
  test("Matrix.test_submatrix2") {
    val s1: String = """| -6 | 1 | 1 | 6 |
                        | -8 | 5 | 8 | 6 |
                        | -1 | 0 | 8 | 2 |
                        | -7 | 1 | -1 | 1 |""".stripMargin
    val res: String = """| -6 | 1 | 6 |
                         | -8 | 8 | 6 |
                         | -7 | -1 | 1 |""".stripMargin

    val m1: Matrix = Matrix.matrixFromString(s1)
    val mres: Matrix = Matrix.matrixFromString(res)

    assert(m1.submatrix(2, 1) === mres)
  }
  test("Matrix.test_minor") {
    val s1: String = """| 3 | 5 | 0 |
                        | 2 | -1 | -7 |
                        | 6 | -1 | 5 |""".stripMargin
    val m1: Matrix = Matrix.matrixFromString(s1)

    val b: Matrix = m1.submatrix(1, 0)
    assert(b.determinant === 25 && m1.minor(1,0) === 25)
  }
  test("Matrix.test_cofactor") {
    val s1: String = """| 3 | 5 | 0 |
                        | 2 | -1 | -7 |
                        | 6 | -1 | 5 |""".stripMargin
    val m1: Matrix = Matrix.matrixFromString(s1)

    assert(m1.minor(0,0) === -12 &&
      m1.cofactor(0,0) === -12 &&
      m1.minor(1, 0) === 25 &&
    m1.cofactor(1, 0) === -25)
  }
  test("Matrix.test_determinant1") {
    val s1: String = """| 1 | 2 | 6 |
                        | -5 | 8 | -4 |
                        | 2 | 6 | 4 |""".stripMargin
    val m1: Matrix = Matrix.matrixFromString(s1)
    assert(m1.cofactor(0,0) === 56 && m1.cofactor(0,1) === 12 &&
      m1.cofactor(0,2) === -46 && m1.determinant === -196 )
  }
  test("Matrix.test_determinant2") {
    val s1: String = """| -2 | -8 | 3 | 5 |
                        | -3 | 1 | 7 | 3 |
                        | 1 | 2 | -9 | 6 |
                        | -6 | 7 | 7 | -9 |""".stripMargin
    val m1: Matrix = Matrix.matrixFromString(s1)
    assert(m1.cofactor(0,0) === 690 && m1.cofactor(0,1) === 447 &&
      m1.cofactor(0,2) === 210 && m1.cofactor(0, 3) === 51 && m1.determinant === -4071 )
  }

  test("Matrix.test_isinvertible1") {
    val s1: String = """| 6 | 4 | 4 | 4 |
                        | 5 | 5 | 7 | 6 |
                        | 4 | -9 | 3 | -7 |
                        | 9 | 1 | 7 | -6 |""".stripMargin
    val m1: Matrix = Matrix.matrixFromString(s1)
    assert(m1.determinant === -2120 && m1.isInvertible)
  }
  test("Matrix.test_isinvertible2") {
    val s1: String = """| -4 | 2 | -2 | -3 |
                        | 9 | 6 | 2 | 6 |
                        | 0 | -5 | 1 | -5 |
                        | 0 | 0 | 0 | 0 |""".stripMargin
    val m1: Matrix = Matrix.matrixFromString(s1)
    assert(m1.determinant === 0 && !(m1.isInvertible) )
  }
  test("Matrix.test_inverse1") {
    val s1: String = """| -5 | 2 | 6 | -8 |
                        | 1 | -5 | 1 | 8 |
                        | 7 | 7 | -6 | -7 |
                        | 1 | -3 | 7 | 4 |""".stripMargin
    val res: String = """|  0.21805 |  0.45113 |  0.24060 | -0.04511 |
                        | -0.80827 | -1.45677 | -0.44361 |  0.52068 |
                        | -0.07895 | -0.22368 | -0.05263 |  0.19737 |
                        | -0.52256 | -0.81391 | -0.30075 |  0.30639 |""".stripMargin

    val m1: Matrix = Matrix.matrixFromString(s1)
    val mres: Matrix = Matrix.matrixFromString(res)
    val b: Matrix = m1.inverse
    assert(m1.determinant === 532 &&
    m1.cofactor(2,3) === -160 &&
    b(3,2) === (-160.0/532) &&
    m1.cofactor(3,2) === 105 &&
    b(2,3) === (105.0/532) &&
    b === mres)
  }
  test("Matrix.test_inverse2") {
    val s1: String = """| 8 | -5 | 9 | 2 |
                        | 7 | 5 | 6 | 1 |
                        | -6 | 0 | 9 | 6 |
                        | -3 | 0 | -9 | -4 |""".stripMargin
    val res: String = """| -0.15385 | -0.15385 | -0.28205 | -0.53846 |
                        | -0.07692 |  0.12308 |  0.02564 |  0.03077 |
                        |  0.35897 |  0.35897 |  0.43590 |  0.92308 |
                        | -0.69231 | -0.69231 | -0.76923 | -1.92308 |""".stripMargin

    val m1: Matrix = Matrix.matrixFromString(s1)
    val mres: Matrix = Matrix.matrixFromString(res)
    assert(m1.inverse === mres)
  }
  test("Matrix.test_inverse3") {
    val s1: String = """| 9 | 3 | 0 | 9 |
                        | -5 | -2 | -6 | -3 |
                        | -4 | 9 | 6 | 4 |
                        | -7 | 6 | 6 | 2 |""".stripMargin
    val res: String = """| -0.04074 | -0.07778 |  0.14444 | -0.22222 |
                        | -0.07778 |  0.03333 |  0.36667 | -0.33333 |
                        | -0.02901 | -0.14630 | -0.10926 |  0.12963 |
                        |  0.17778 |  0.06667 | -0.26667 |  0.33333 |""".stripMargin

    val m1: Matrix = Matrix.matrixFromString(s1)
    val mres: Matrix = Matrix.matrixFromString(res)
    assert(m1.inverse === mres)
  }
  test("Matrix.test_inverse4") {
    val s1: String = """| 3 | -9 | 7 | 3 |
                        | 3 | -8 | 2 | -9 |
                        | -4 | 4 | 4 | 1 |
                        | -6 | 5 | -1 | 1 |""".stripMargin
    val s2: String = """| 8 | 2 | 2 | 2 |
                        | 3 | -1 | 7 | 0 |
                        | 7 | 0 | 5 | 4 |
                        | 6 | -2 | 0 | 4 |""".stripMargin

    val m1: Matrix = Matrix.matrixFromString(s1)
    val m2: Matrix = Matrix.matrixFromString(s2)
    val c: Matrix = m1 * m2
    assert(c * m2.inverse === m1)
  }

  test("Matrix.test_inverse5") {
    assert(Matrix.getIdentityMatrix(4).inverse === Matrix.getIdentityMatrix(4))
  }

  test("Matrix.test_inverse6") {
    val s1: String = """| 3 | -9 | 7 | 3 |
                        | 3 | -8 | 2 | -9 |
                        | -4 | 4 | 4 | 1 |
                        | -6 | 5 | -1 | 1 |""".stripMargin
    val m1: Matrix = Matrix.matrixFromString(s1)
    assert(m1 * m1.inverse === Matrix.getIdentityMatrix(4))
  }
  test("Matrix.test_inverse7") {
    val s1: String = """| 3 | -9 | 7 | 3 |
                        | 3 | -8 | 2 | -9 |
                        | -4 | 4 | 4 | 1 |
                        | -6 | 5 | -1 | 1 |""".stripMargin
    val m1: Matrix = Matrix.matrixFromString(s1)
    assert(m1.transpose.inverse === m1.inverse.transpose)
  }
  test("Matrix.test_tuple") {
    val s1: String = """| 5 | 0 | 0 | 0 |
                        | 0 | 1 | 0 | 0 |
                        | 0 | 0 | 1 | 0 |
                        | 0 | 0 | 0 | 1 |""".stripMargin
    val m1: Matrix = Matrix.matrixFromString(s1)
    val tuple1: RTTuple = Point(1,2,3)
    assert(m1.tupleMult(tuple1).toTuple === (5,2,3,1))
  }

  }
