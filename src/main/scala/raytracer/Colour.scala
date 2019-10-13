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

class Colour(val red: Double, val green: Double, val blue: Double){
  def toTuple: (Double, Double, Double) = (red, green, blue)

  final override def equals(that: Any): Boolean = {
    that match {
      case that: Colour => doubleEq(that.red, red) && doubleEq(that.green, green) && doubleEq(that.blue, blue)
      case _ => false
    }
  }

  final override def hashCode: Int = (red, green, blue).##

  def +(that: Colour): Colour = Colour(red + that.red, green + that.green, blue + that.blue)

  def -(that: Colour): Colour = Colour(red - that.red, green - that.green, blue - that.blue)

  def *(that: Double): Colour = Colour(red * that, green * that, blue * that)

  def **(that: Colour): Colour = Colour(red * that.red, green * that.green, blue * that.blue)

  def /(that: Double): Colour = Colour(red / that, green / that, blue / that)

}

object Colour {
  def apply(red: Double, green: Double, blue: Double): Colour = new Colour(red, green, blue)
}
