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

class Intersection(val t: Double, val shape: SpaceObject) {

}

object Intersection {
  def intersections(is: Intersection*): Seq[Intersection] = {
    is
  }
  def hit(xs: Seq[Intersection]): Intersection = {
    // TODO: Use better WartRemover hack
    xs.filter(_.t>0).foldLeft(new Intersection(99999999, Sphere.unitSphere()))((s: Intersection, x: Intersection) => if (x.t < s.t) x else s )
  }
}