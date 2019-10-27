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

import cats.implicits._

class World(val lights: Seq[Light], val shapes: Seq[SpaceObject]) {
  def setLights(l: Seq[Light]): World = {
    new World(l, shapes)
  }
  def setShapes(s: Seq[SpaceObject]): World = {
    new World(lights, s)
  }

  def intersectWorld(r: Ray): Seq[Intersection] = {
    shapes.par.flatMap((x: SpaceObject) => x.intersect(r)).seq.sortBy((z: Intersection) => z.t)
  }

  def shadeHit(comps: Computation): Colour = {
    lights.par.map(comps.shape.material.lighting(_, comps.point, comps.eyev, comps.normalv)).reduce(_ + _)
  }

  def colourAt(r: Ray): Colour = {
    Intersection.hit(intersectWorld(r)) match {
        // TODO: Swap hack for Option
      case x: Intersection if x.t === 99999999 => Colour.black
      case x: Intersection => shadeHit(Computation.prepareComputations(x, r))
    }
  }
}

object World {
  def emptyWorld: World = new World(Array[Light](), Array[SpaceObject]())
  def defaultWorld: World = {
    val m1: Material = new Material(Colour(0.8, 1.0, 0.6), 0.1, 0.7, 0.2, 200.0)
    val s1: Sphere = Sphere.unitSphere().setMaterial(m1)
    val s2: Sphere = Sphere.unitSphere().setTransform(Scaling(0.5, 0.5, 0.5))
    val light: Light = Light.pointLight(Point(-10, 10, -10), Colour(1,1,1))
    new World(Array[Light](light), Array[SpaceObject](s1, s2))
  }

  def apply(lights: Seq[Light], shapes: Seq[SpaceObject]): World = new World(lights, shapes)
}