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

class CameraTest extends FunSuite {

  test("Camera.test_creation1") {
    val c: Camera = Camera(160, 120, math.Pi / 2)

    assert(c.hsize === 160 && c.vsize === 120 && c.fov === math.Pi / 2 && c.transform === Matrix.getIdentityMatrix(4))
  }
  test("Camera.test_pixelsize_horizontal") {
    val c: Camera = Camera(200, 125, math.Pi / 2)

    assert(doubleEq(c.pixel_size, 0.01))
  }
  test("Camera.test_pixelsize_vertical") {
    val c: Camera = Camera(125, 200, math.Pi / 2)

    assert(doubleEq(c.pixel_size, 0.01))
  }
  test("Camera.test_raypixel_1") {
    val c: Camera = Camera(201, 101, math.Pi / 2)
    val r: Ray = c.rayForPixel(100, 50)

    assert(r.origin === Point(0, 0, 0) && r.direction === Vector(0, 0, -1))
  }
  test("Camera.test_raypixel_2") {
    val c: Camera = Camera(201, 101, math.Pi / 2)
    val r: Ray = c.rayForPixel(0, 0)

    assert(r.origin === Point(0, 0, 0) && r.direction === Vector(0.66519, 0.33259, -0.66851))
  }
  test("Camera.test_raypixel_3") {
    val c: Camera = Camera(201, 101, math.Pi / 2).setTransform(RotationY(math.Pi / 4) * Translation(0, -2, 5))
    val r: Ray = c.rayForPixel(100, 50)

    assert(r.origin === Point(0, 2, -5) && r.direction === Vector(math.sqrt(2) / 2, 0, -math.sqrt(2) / 2))
  }
  test("Camera.test_render") {
    val w: World = World.defaultWorld
    val c: Camera = Camera(11, 11, math.Pi / 2).setTransform(viewTransform(Point(0, 0, -5), Point(0, 0, 0), Vector(0, 1, 0)))
    val image: Canvas = c.render(w)

    assert(image.pixelAt(5, 5) === Colour(0.38066, 0.47583, 0.2855))
  }
}
