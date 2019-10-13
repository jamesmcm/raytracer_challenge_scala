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

class CanvasTest extends FunSuite {

  test("Canvas.test_creation1") {
    val c = new Canvas(10, 20)
    assert(c.width === 10 && c.height === 20)
  }
  test("Canvas.test_creation2") {
    val c = new Canvas(10, 20)
    assert(c.pixels.forall((x: Array[Colour]) => x.forall(_ === Colour(0,0,0))))
  }
  test("Canvas.test_write_pixel") {
    val c = new Canvas(10, 20)
    val r = Colour(1, 0, 0)
    c.writePixel(2, 3, r)
    assert(c.pixelAt(2, 3) === r)
  }

  test("Canvas.test_ppm_header") {
    val c = new Canvas(5, 3)
    assert(c.toPPM.split("\n").take(3).mkString("\n") === "P3\n5 3\n255")
  }
  test("Canvas.test_ppm_body") {
    val c = new Canvas(5, 3)
    val col1 = Colour(1.5, 0, 0)
    val col2 = Colour(0, 0.5, 0)
    val col3 = Colour(-0.5, 0, 1)

    c.writePixel(0, 0, col1)
    c.writePixel(2, 1, col2)
    c.writePixel(4, 2, col3)
    println(c.toPPM.split("\n").slice(3,6).mkString("\n"))
    assert(c.toPPM.split("\n").slice(3,6).mkString("\n") === "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 128 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0 0 0 0 0 0 255")
  }
  test("Canvas.test_remove_last_space") {
    assert(Canvas.replaceLastSpaceWithNewline("test string") === "test string")
  }
  test("Canvas.test_ppm_body_long") {
    val c = new Canvas(10, 2)
    val col1 = Colour(1, 0.8, 0.6)

    c.fillCanvas(col1)
    println(c.toPPM.split("\n").slice(3,7).mkString("\n"))
    assert(c.toPPM.split("\n").slice(3,7).mkString("\n") === "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n153 255 204 153 255 204 153 255 204 153 255 204 153\n255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n153 255 204 153 255 204 153 255 204 153 255 204 153")
  }
  test("Canvas.test_ppm_newline") {
    val c = new Canvas(5, 3)
    assert(c.toPPM(c.toPPM.length-1) === '\n')
  }

}
