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

  def shadeHit(comps: Computation, remaining: Int): Colour = {
    val base: Colour = lights.par
      .map(
        (l: Light) =>
          comps.shape.material.lighting(comps.shape,
                                        l,
                                        comps.point,
                                        comps.eyev,
                                        comps.normalv,
                                        isShadowed(comps.over_point, l)))
      .reduce(_ + _)

    comps.shape.material match {
      case m: Material if m.reflective > 0 && m.transparency > 0 =>
        base + (reflectedColour(comps, remaining) * comps
          .schlick()) + (refractedColour(comps, remaining) * (1 - comps.schlick()))
      case _ => base + reflectedColour(comps, remaining) + refractedColour(comps, remaining)
    }
  }

  def colourAt(r: Ray, remaining: Int): Colour = {
    val intersections: Seq[Intersection] = intersectWorld(r)
    Intersection.hit(intersections) match {
      case None => Colour.black
      case Some(x: Intersection) =>
        shadeHit(Computation.prepareComputations(x, r, intersections), remaining)
    }
  }

  def isShadowed(p: RTTuple, l: Light): Boolean = {
    // Handle multiple lights
    val v: RTTuple         = l.position - p
    val distance: Double   = v.magnitude()
    val direction: RTTuple = v.normalise()

    val h: Option[Intersection] = Intersection.hit(intersectWorld(Ray(p, direction)))

    h match {
      case None                  => false
      case Some(x: Intersection) => x.t < distance && x.shape.shadow
    }
  }

  def reflectedColour(comps: Computation, remaining: Int): Colour = {
    if (remaining < 1) { Colour(0, 0, 0) } else {
      if (comps.shape.material.reflective === 0) { Colour(0, 0, 0) } else {
        val reflect_ray: Ray = Ray(comps.over_point, comps.reflectv)
        colourAt(reflect_ray, remaining - 1) * comps.shape.material.reflective
      }
    }
  }

  def refractedColour(comps: Computation, remaining: Int): Colour = {
    if (remaining < 1) {
      Colour(0, 0, 0)
    } else {
      if (comps.shape.material.transparency === 0) {
        Colour(0, 0, 0)
      } else {
        val n_ratio: Double = comps.n1 / comps.n2
        val cos_i: Double   = comps.eyev.dot(comps.normalv)
        val sin2_t: Double  = (n_ratio * n_ratio) * (1 - (cos_i * cos_i))
        if (sin2_t > 1) { Colour(0, 0, 0) } else {
          val cos_t: Double      = math.sqrt(1.0 - sin2_t)
          val direction: RTTuple = comps.normalv * (n_ratio * cos_i - cos_t) - comps.eyev * n_ratio
          val refract_ray: Ray   = Ray(comps.under_point, direction)
          colourAt(refract_ray, remaining - 1) * comps.shape.material.transparency
        }
      }
    }
  }
}

object World {
  def emptyWorld: World = new World(Array[Light](), Array[SpaceObject]())

  def defaultWorld: World = {
    val m1: Material = new Material(Colour(0.8, 1.0, 0.6), 0.1, 0.7, 0.2, 200.0, None, 0, 0, 1)
    val s1: Sphere   = Sphere.unitSphere().setMaterial(m1)
    val s2: Sphere   = Sphere.unitSphere().setTransform(Scaling(0.5, 0.5, 0.5))
    val light: Light = Light.pointLight(Point(-10, 10, -10), Colour(1, 1, 1))
    new World(Array[Light](light), Array[SpaceObject](s1, s2))
  }

  def apply(lights: Seq[Light], shapes: Seq[SpaceObject]): World = new World(lights, shapes)
}
