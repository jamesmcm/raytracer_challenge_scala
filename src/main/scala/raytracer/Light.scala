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

class Light(val position: RTTuple, val intensity: Colour) {
  final override def equals(that: Any): Boolean = {
    that match {
      case that: Light => position === that.position && intensity === that.intensity
      case _           => false
    }
  }

  final def ===(that: Light): Boolean = {
    position === that.position && intensity === that.intensity
  }

  final override def hashCode: Int = (position, intensity).##
}

object Light {
  def pointLight(position: RTTuple, intensity: Colour): Light = {
    new Light(position, intensity)
  }
}
