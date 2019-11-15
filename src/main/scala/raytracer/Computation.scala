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

import scala.annotation.tailrec

class Computation(val t: Double,
                  val shape: SpaceObject,
                  val point: RTTuple,
                  val eyev: RTTuple,
                  val normalv: RTTuple,
                  val inside: Boolean,
                  val over_point: RTTuple,
                  val reflectv: RTTuple,
                  val n1: Double,
                  val n2: Double) {}

object Computation {

  @tailrec
  def recurseRefract(hit: Intersection,
                     xs: Seq[Intersection],
                     containers: Seq[SpaceObject]): (Double, Double) = {
    xs.headOption match {
      case None => (99.0, 99.0)
      case Some(x: Intersection) if x === hit => {
        var n1: Double = 1;
        var containersNew: Seq[SpaceObject] = containers
        if (containers.isEmpty) { n1 = 1.0 } else {
          n1 = containers.reverse(0).material.refractiveIndex
        }
        if (containers.contains(x.shape)) {
          containersNew =
            containers.filterNot((z: SpaceObject) => z === x.shape)
        } else {containersNew = containers :+ x.shape }
        if (containersNew.isEmpty) {(n1, 1.0)} else {(n1, containersNew.reverse(0).material.refractiveIndex)}
      }
      case Some(y: Intersection) => {
        if (containers.contains(y.shape)) {
            recurseRefract(hit, xs.drop(1), containers.filterNot((z: SpaceObject) => z === y.shape))
        } else { recurseRefract(hit, xs.drop(1), containers :+ y.shape) }
      }
    }
  }

  def prepareComputations(intersection: Intersection,
                          ray: Ray,
                          xs: Seq[Intersection]): Computation = {
    val normalv: RTTuple = intersection.shape.normalAt(ray.position(intersection.t))
    val eyev: RTTuple    = ray.direction.negate()

    val hit: Intersection = intersection //Intersection.hit(xs) wtf
    val (n1: Double, n2: Double) = recurseRefract(hit, xs, List())

    // TODO: Refactor me to handle negation only once
    if (normalv.dot(eyev) < 0) {
      new Computation(
        intersection.t,
        intersection.shape,
        ray.position(intersection.t),
        eyev,
        normalv.negate(),
        true,
        ray.position(intersection.t) + normalv.negate() * EPSILON,
        ray.direction.reflect(normalv.negate()), n1, n2
      )
    } else {
      new Computation(
        intersection.t,
        intersection.shape,
        ray.position(intersection.t),
        eyev,
        normalv,
        false,
        ray.position(intersection.t) + normalv * EPSILON,
        ray.direction.reflect(normalv), n1, n2
      )
    }
  }
}
