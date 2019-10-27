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

class Ray(val origin: RTTuple, val direction: RTTuple) {
  def position(t: Double): RTTuple = {
    origin + (direction * t)
  }

  def transform(m: Matrix): Ray = {
    Ray(m.tupleMult(origin), m.tupleMult(direction))
  }
}

object Ray{
  def apply(origin: RTTuple,  direction: RTTuple): Ray = new Ray(origin, direction)
}