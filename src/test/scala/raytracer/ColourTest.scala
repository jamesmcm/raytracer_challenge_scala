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

class ColourTest extends FunSuite {
  test("Colour.test_components") {
    val c: Colour = Colour(-0.5, 0.4, 1.7)
    assert(c.red === -0.5 && c.green === 0.4 && c.blue === 1.7)
  }

  test("Colour.test_add") {
    assert(Colour(0.9, 0.6, 0.75) + Colour(0.7, 0.1, 0.25) === Colour(1.6, 0.7, 1.0))
  }
  test("Colour.test_sub") {
    assert(Colour(0.9, 0.6, 0.75) - Colour(0.7, 0.1, 0.25) === Colour(0.2, 0.5, 0.5))
  }
  test("Colour.test_mult") {
    assert(Colour(0.2, 0.3, 0.4) * 2 === Colour(0.4, 0.6, 0.8))
  }
  test("Colour.test_hadamard") {
    assert((Colour(1, 0.2, 0.4) ** Colour(0.9, 1, 0.1)) === Colour(0.9, 0.2, 0.04))
  }

}
