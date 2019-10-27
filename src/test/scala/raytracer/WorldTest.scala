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

class WorldTest extends FunSuite {
  test("World.test_creation") {
    val w: World = World.emptyWorld
    assert(w.lights.length === 0 && w.shapes.length === 0)
  }
  test("World.test_default") {
    val w: World = World.defaultWorld
    val light: Light = Light.pointLight(Point(-10, 10, -10), Colour(1, 1, 1))
    val m1: Material = new Material(Colour(0.8, 1.0, 0.6), 0.1, 0.7, 0.2, 200.0)
    val s1: Sphere = Sphere.unitSphere().setMaterial(m1)
    val s2: Sphere = Sphere.unitSphere().setTransform(Scaling(0.5, 0.5, 0.5))

    assert(w.lights.contains(light) && w.shapes.contains(s1) && w.shapes.contains(s2))
  }
  test("World.test_intersect") {
    val w: World = World.defaultWorld
    val r: Ray = Ray(Point(0,0,-5), Vector(0,0,1))
    val xs: Seq[Intersection] = w.intersectWorld(r)
    assert(xs.length === 4 && xs(0).t === 4 && xs(1).t === 4.5 && xs(2).t === 5.5 && xs(3).t === 6)
  }

  test("World.shade_intersection_outside") {
    val w: World = World.defaultWorld
    val r: Ray = Ray(Point(0,0,-5), Vector(0,0,1))
    val s: SpaceObject = w.shapes(0)
    val i: Intersection = new Intersection(4, s)

    val comps: Computation = Computation.prepareComputations(i, r)
    val c: Colour = w.shadeHit(comps)
    assert(c === Colour(0.38066, 0.47583, 0.2855))
  }

  test("World.shade_intersection_inside") {
    val w: World = World.defaultWorld.setLights(List(Light.pointLight(Point(0, 0.25, 0), Colour(1, 1, 1))))
    val r: Ray = Ray(Point(0,0,0), Vector(0,0,1))
    val s: SpaceObject = w.shapes(1)
    val i: Intersection = new Intersection(0.5, s)

    val comps: Computation = Computation.prepareComputations(i, r)
    val c: Colour = w.shadeHit(comps)
    assert(c === Colour(0.90498,0.90498,0.90498))
  }

  test("World.colour_miss") {
    val w: World = World.defaultWorld
    val r: Ray = Ray(Point(0,0,-5), Vector(0,1,0))

    val c: Colour = w.colourAt(r)
    assert(c === Colour.black)
  }
  test("World.colour_hit") {
    val w: World = World.defaultWorld
    val r: Ray = Ray(Point(0,0,-5), Vector(0,0,1))

    val c: Colour = w.colourAt(r)
    assert(c === Colour(0.38066, 0.47583, 0.2855))
  }
  test("World.colour_intersection_behind") {
    val m: Material = Material.defaultMaterial().setAmbient(1)
    val w: World = World.defaultWorld.setShapes(World.defaultWorld.shapes.map(_.setMaterial(m)))
    val r: Ray = Ray(Point(0,0,0.75), Vector(0,0,-1))
    val inner: SpaceObject = w.shapes(1)

    val c: Colour = w.colourAt(r)
    assert(c === inner.material.colour)
  }
}
