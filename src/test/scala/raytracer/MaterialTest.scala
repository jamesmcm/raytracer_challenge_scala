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

class MaterialTest extends FunSuite {
  test("Material.test_creation") {
    val m: Material = Material.defaultMaterial()
    assert(m.colour === Colour(1, 1, 1) && m.ambient === 0.1 && m.diffuse === 0.9 &&
      m.specular === 0.9 && m.shininess === 200.0)
  }
  test("Material.test_lighting1") {
    val m: Material = Material.defaultMaterial()
    val p: RTTuple = Point(0, 0, 0)

    val eyev: RTTuple = Vector(0, 0, -1)
    val normalv: RTTuple = Vector(0, 0, -1)
    val light: Light = Light.pointLight(Point(0, 0, -10), Colour(1, 1, 1))

    val result: Colour = m.lighting(Sphere.unitSphere(), light, p, eyev, normalv, false)
    assert(result === Colour(1.9, 1.9, 1.9))
  }
  test("Material.test_lighting2") {
    val m: Material = Material.defaultMaterial()
    val p: RTTuple = Point(0, 0, 0)

    val eyev: RTTuple = Vector(0, math.sqrt(2) / 2, -math.sqrt(2) / 2)
    val normalv: RTTuple = Vector(0, 0, -1)
    val light: Light = Light.pointLight(Point(0, 0, -10), Colour(1, 1, 1))

    val result: Colour = m.lighting(Sphere.unitSphere(), light, p, eyev, normalv, false)
    assert(result === Colour(1.0, 1.0, 1.0))
  }
  test("Material.test_lighting3") {
    val m: Material = Material.defaultMaterial()
    val p: RTTuple = Point(0, 0, 0)

    val eyev: RTTuple = Vector(0, 0, -1)
    val normalv: RTTuple = Vector(0, 0, -1)
    val light: Light = Light.pointLight(Point(0, 10, -10), Colour(1, 1, 1))

    val result: Colour = m.lighting(Sphere.unitSphere(), light, p, eyev, normalv, false)
    assert(result === Colour(0.7364, 0.7364, 0.7364))
  }
  test("Material.test_lighting4") {
    val m: Material = Material.defaultMaterial()
    val p: RTTuple = Point(0, 0, 0)

    val eyev: RTTuple = Vector(0, -math.sqrt(2) / 2, -math.sqrt(2) / 2)
    val normalv: RTTuple = Vector(0, 0, -1)
    val light: Light = Light.pointLight(Point(0, 10, -10), Colour(1, 1, 1))

    val result: Colour = m.lighting(Sphere.unitSphere(), light, p, eyev, normalv, false)
    assert(result === Colour(1.6364, 1.6364, 1.6364))
  }
  test("Material.test_lighting5") {
    val m: Material = Material.defaultMaterial()
    val p: RTTuple = Point(0, 0, 0)

    val eyev: RTTuple = Vector(0, 0, -1)
    val normalv: RTTuple = Vector(0, 0, -1)
    val light: Light = Light.pointLight(Point(0, 0, 10), Colour(1, 1, 1))

    val result: Colour = m.lighting(Sphere.unitSphere(), light, p, eyev, normalv, false)
    assert(result === Colour(0.1, 0.1, 0.1))
  }
  test("Material.test_lighting_shadow1") {
    val m: Material = Material.defaultMaterial()
    val p: RTTuple = Point(0, 0, 0)

    val eyev: RTTuple = Vector(0, 0, -1)
    val normalv: RTTuple = Vector(0, 0, -1)
    val light: Light = Light.pointLight(Point(0, 0, -10), Colour(1, 1, 1))

    val result: Colour = m.lighting(Sphere.unitSphere(), light, p, eyev, normalv, true)
    assert(result === Colour(0.1, 0.1, 0.1))
  }
  test("Material.test_pattern1") {
    val m: Material = Material.defaultMaterial().setDiffuse(0).setSpecular(0).setAmbient(1)
      .setPattern((StripePattern(Colour(1,1,1), Colour(0,0,0))))

    val eyev: RTTuple = Vector(0, 0, -1)
    val normalv: RTTuple = Vector(0, 0, -1)
    val light: Light = Light.pointLight(Point(0, 0, -10), Colour(1, 1, 1))

    val c1: Colour = m.lighting(Sphere.unitSphere(), light, Point(0.9, 0, 0), eyev, normalv, false)
    val c2: Colour = m.lighting(Sphere.unitSphere(), light, Point(1.1, 0, 0), eyev, normalv, false)

    assert(c1 === Colour(1,1,1) && c2 === Colour(0,0,0))
  }
}
