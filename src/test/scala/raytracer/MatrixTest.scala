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
  test("Colour.test_construction1") {
    val s: String =
      """| 1 | 2 | 3 | 4 |
         | 5.5 | 6.5 | 7.5 | 8.5 |
         | 9 | 10 | 11 | 12 |
         | 13.5 | 14.5 | 15.5 | 16.5 |""".stripMargin

    val m: Matrix = Matrix.matrixFromString(s)
    assert(m.m(0)(0) === 1 && m.m(0)(3) === 4 && m.m(1)(0) === 5.5 && m.m(1)(2) === 7.5 &&
      m.m(2)(2) === 11 && m.m(3)(0) === 13.5 && m.m(3)(2) === 15.5)
  }
  test("Colour.test_construction2") {
    val s: String =
      """| -3 | 5 |
         | 1 | -2 |""".stripMargin

    val m: Matrix = Matrix.matrixFromString(s)
    assert(m(0,0) === -3 && m(0,1) === 5 && m(1,0) === 1 && m(1,1) === -2)
  }
  test("Colour.test_construction3") {
    val s: String =
      """|-3|5|0|
         |1|-2|-7|
         |0|1|1|""".stripMargin

    val m: Matrix = Matrix.matrixFromString(s)
    assert(m(0,0) === -3 && m(1,1) === -2 && m(2,2) === 1)
  }
  test("Colour.test_equality1") {
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
  test("Colour.test_equality2") {
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
  }
