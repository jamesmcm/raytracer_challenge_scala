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

class Computation(val t: Double, val shape: SpaceObject, val point: RTTuple, val eyev: RTTuple, val normalv: RTTuple,
                 val inside: Boolean) {

}

object Computation {
  def prepareComputations(intersection: Intersection, ray: Ray): Computation = {
    val normalv: RTTuple = intersection.shape.normalAt(ray.position(intersection.t))
    val eyev: RTTuple = ray.direction.negate()
    if (normalv.dot(eyev) < 0) {new Computation(intersection.t, intersection.shape,
      ray.position(intersection.t), eyev, normalv.negate(), true)} else {new Computation(intersection.t,
      intersection.shape, ray.position(intersection.t), eyev, normalv, false)}
  }
}
